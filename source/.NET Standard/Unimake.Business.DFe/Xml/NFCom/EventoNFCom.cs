#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Xml;
using System.Text;
using Unimake.Business.DFe.Utility;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Schema;
using System.Xml.Linq;
using System.Globalization;

namespace Unimake.Business.DFe.Xml.NFCom
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.EventoNFCom")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eventoNFCom", Namespace = "http://www.portalfiscal.inf.br/nfcom", IsNullable = false)]
    public class EventoNFCom : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("infEvento")]
        public InfEvento InfEvento { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

        #region Public Methods

        /// <summary>
        /// Desserializar o XML no objeto EventoNFCom
        /// </summary>
        /// <param name="filename">Localização do arquivo XML do EventoNFCom</param>
        /// <returns>Objeto do EventoNFCom</returns>
        public EventoNFCom LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<EventoNFCom>(doc);
        }

        /// <summary>
        /// Desserializar o XML EventoNFCom no objeto EventoNFCom
        /// </summary>
        /// <param name="xml">string do XML EventoNFCom</param>
        /// <returns>Objeto do EventoNFCom</returns>
        public EventoNFCom LoadFromXML(string xml) => XMLUtility.Deserializar<EventoNFCom>(xml);

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.InfEvento")]
    [ComVisible(true)]
#endif
    public class InfEvento
    {
        #region Private Fields

        private EventoDetalhe _detEvento;

        #endregion Private Fields

        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id
        {
            get => "ID" + ((int)TpEvento).ToString() + ChNFCom + NSeqEvento.ToString("000");
            set => _ = value;
        }

        [XmlIgnore]
        public UFBrasil COrgao { get; set; }

        [XmlElement("cOrgao")]
        public int COrgaoField
        {
            get => (int)COrgao;
            set => COrgao = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("chNFCom")]
        public string ChNFCom { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhEvento { get; set; }
#else
        public DateTimeOffset DhEvento { get; set; }
#endif

        [XmlElement("dhEvento")]
        public string DhEventoField
        {
            get => DhEvento.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhEvento = DateTime.Parse(value);
#else
            set => DhEvento = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tpEvento")]
        public TipoEventoNFCom TpEvento { get; set; }

        [XmlElement("nSeqEvento")]
        public int NSeqEvento { get; set; }

        [XmlElement("detEvento")]
        public EventoDetalhe DetEvento
        {
            get => _detEvento;
            set
            {
                switch (TpEvento)
                {
                    case 0:
                        _detEvento = value;
                        break;

                    case TipoEventoNFCom.Cancelamento:
                        _detEvento = new DetEventoCanc();
                        break;

                    case TipoEventoNFCom.VinculacaoPagamento:
                        _detEvento = new DetEventoVincPgto();
                        break;

                    case TipoEventoNFCom.CancelamentoVinculacaoPagamento:
                        _detEvento = new DetEventoCancVincPgto();
                        break;

                    case TipoEventoNFCom.TributacaoSimplesNacional:
                        _detEvento = new DetEventoTributacaoSN();
                        break;

                    case TipoEventoNFCom.CancelamentoTributacaoSimplesNacional:
                        _detEvento = new DetEventoCancTributacaoSN();
                        break;

                    default:
                        throw new NotImplementedException($"O tipo de evento '{TpEvento}' não está implementado.");
                }

                _detEvento.XmlReader = value.XmlReader;
                _detEvento.ProcessReader();
            }
        }

        public InfEvento() { }

        public InfEvento(EventoDetalhe detEvento) => DetEvento = detEvento ?? throw new ArgumentNullException(nameof(detEvento));
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.EventoDetalhe")]
    [ComVisible(true)]
#endif
    public class EventoDetalhe : IXmlSerializable
    {
        #region Internal Properties

        internal XmlReader XmlReader { get; set; }

        private static readonly BindingFlags bindingFlags = BindingFlags.Public | BindingFlags.Instance | BindingFlags.IgnoreCase;

        private static readonly List<string> hasField = new List<string>
        {
            "COrgaoAutor",
        };

        #endregion Internal Properties

        #region Internal Methods

        internal virtual void ProcessReader()
        {
            if (XmlReader == null)
            {
                return;
            }

            var type = GetType();

            if (XmlReader.HasAttributes)
            {
                if (XmlReader.GetAttribute("versaoEvento") != "")
                {
                    var pi = type.GetProperty("versaoEvento", BindingFlags.Public | BindingFlags.Instance | BindingFlags.IgnoreCase);
                    pi?.SetValue(this, XmlReader.GetAttribute("versaoEvento"));
                }
            }

            while (XmlReader.Read())
            {
                if (XmlReader.NodeType != XmlNodeType.Element)
                {
                    continue;
                }

                SetValue(type);
            }
        }

        internal virtual void SetValue(Type type)
        {
            var pi = GetPropertyInfo(type);

            if (pi == null)
            {
                return;
            }

            SetValue(pi);
        }

        internal virtual void SetValue(PropertyInfo pi) => pi?.SetValue(this, Converter.ToAny(XmlReader.GetValue<object>(XmlReader.Name), pi.PropertyType));

        protected internal PropertyInfo GetPropertyInfo(Type type)
        {
            var pi = hasField.Exists(w => w.ToLower() == XmlReader.Name.ToLower()) ? type.GetProperty(XmlReader.Name + "Field", bindingFlags) : type.GetProperty(XmlReader.Name, bindingFlags);

            return pi;
        }

        #endregion Internal Methods

        #region Public Properties

        [XmlElement("descEvento")]
        public virtual string DescEvento { get; set; }

        [XmlAttribute(AttributeName = "versaoEvento", DataType = "token")]
        public virtual string VersaoEvento { get; set; }

        #endregion Public Properties

        #region Public Methods

        public XmlSchema GetSchema() => default;

        public void ReadXml(XmlReader reader) => XmlReader = reader;

        public virtual void WriteXml(XmlWriter writer) => writer.WriteAttributeString("versaoEvento", VersaoEvento);

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.DetEventoCanc")]
    [ComVisible(true)]
#endif
    public class DetEventoCanc : EventoDetalhe
    {
        [XmlAttribute("versaoEvento", DataType = "string")]
        public override string VersaoEvento { get; set; }

        [XmlElement("descEvento")]
        public override string DescEvento { get; set; } = "Cancelamento";

        [XmlElement("nProt")]
        public string NProt { get; set; }

        [XmlElement("xJust")]
        public string XJust { get; set; }

        #region Public Methods

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            writer.WriteRaw($@"
            <evCancNFCom>
            <descEvento>{DescEvento}</descEvento>
            <nProt>{NProt}</nProt>
            <xJust>{XJust}</xJust>
            </evCancNFCom>");
        }

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.DetEventoVincPgto")]
    [ComVisible(true)]
#endif
    public class DetEventoVincPgto : EventoDetalhe
    {
        [XmlAttribute("versaoEvento", DataType = "string")]
        public override string VersaoEvento { get; set; }

        [XmlElement("descEvento")]
        public override string DescEvento { get; set; } = "Vinculacao do Pagamento";

        [XmlElement("nProt")]
        public string NProt { get; set; }

        [XmlElement("pgto")]
        public Pgto Pgto { get; set; }

        #region Internal Methods

        internal override void ProcessReader()
        {
            if (XmlReader == null)
            {
                return;
            }

            if (XmlReader.HasAttributes && XmlReader.GetAttribute("versaoEvento") != "")
            {
                VersaoEvento = XmlReader.GetAttribute("versaoEvento");
            }

            while (XmlReader.Read())
            {
                if (XmlReader.NodeType != XmlNodeType.Element)
                {
                    continue;
                }

                switch (XmlReader.Name)
                {
                    case "descEvento":
                        DescEvento = XmlReader.GetValue<string>(XmlReader.Name);
                        break;

                    case "nProt":
                        NProt = XmlReader.GetValue<string>(XmlReader.Name);
                        break;

                    case "pgto":
                        var pgtoElement = XElement.Parse(XmlReader.ReadOuterXml());

                        Pgto = new Pgto
                        {
                            NPag = pgtoElement.Attribute("nPag")?.Value,
                            IdTransacao = pgtoElement.Attribute("idTransacao")?.Value
                        };

                        foreach (var child in pgtoElement.Elements())
                        {
                            switch (child.Name.LocalName)
                            {
                                case "tpMeioPgto":
                                    Pgto.TpMeioPgto = child.Value;
                                    break;

                                case "CNPJReceb":
                                    Pgto.CNPJReceb = child.Value;
                                    break;

                                case "CNPJBasePSP":
                                    Pgto.CNPJBasePSP = child.Value;
                                    break;
                            }
                        }
                        break;
                }
            }
        }

        #endregion Internal Methods

        #region Public Methods

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            writer.WriteRaw($@"
            <evVincPgto>
            <descEvento>{DescEvento}</descEvento>
            <nProt>{NProt}</nProt>
            <pgto nPag=""{Pgto?.NPag}"" idTransacao=""{Pgto?.IdTransacao}"">
            <tpMeioPgto>{Pgto?.TpMeioPgto}</tpMeioPgto>
            <CNPJReceb>{Pgto?.CNPJReceb}</CNPJReceb>
            <CNPJBasePSP>{Pgto?.CNPJBasePSP}</CNPJBasePSP>
            </pgto>
            </evVincPgto>");
        }

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.DetEventoCancVincPgto")]
    [ComVisible(true)]
#endif
    public class DetEventoCancVincPgto : EventoDetalhe
    {
        [XmlAttribute("versaoEvento", DataType = "string")]
        public override string VersaoEvento { get; set; }

        [XmlElement("descEvento")]
        public override string DescEvento { get; set; } = "Cancelamento da vinculacao do pagamento";

        [XmlElement("nProt")]
        public string NProt { get; set; }

        [XmlElement("nProtVincPgto")]
        public string NProtVincPgto { get; set; }

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            writer.WriteRaw($@"
            <evCancVincPgto>
            <descEvento>{DescEvento}</descEvento>
            <nProt>{NProt}</nProt>
            <nProtVincPgto>{NProtVincPgto}</nProtVincPgto>
            </evCancVincPgto>");
        }
    }

    /// <summary>Evento 412140: tributação do Simples Nacional.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.DetEventoTributacaoSN")]
    [ComVisible(true)]
#endif
    public class DetEventoTributacaoSN : EventoDetalhe
    {
        /// <summary>Descrição do evento.</summary>
        public override string DescEvento { get; set; } = "Tributação do Simples Nacional";

        /// <summary>Protocolo de autorização do documento.</summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>Tributação por item, até 990 ocorrências.</summary>
        [XmlElement("gTribItemSN")]
        public List<GTribItemSN> GTribItemSN { get; set; }

        /// <summary>Totais do Simples Nacional.</summary>
        [XmlElement("gTotalSN")]
        public GTotalSN GTotalSN { get; set; }

#if INTEROP
        /// <summary>Adiciona a tributação de um item.</summary>
        public void AddGTribItemSN(GTribItemSN item)
        {
            if (GTribItemSN == null)
            {
                GTribItemSN = new List<GTribItemSN>();
            }
            GTribItemSN.Add(item);
        }

        /// <summary>Retorna a tributação de um item pelo índice.</summary>
        public GTribItemSN GetGTribItemSN(int index) => GTribItemSN == null || GTribItemSN.Count == 0 ? null : GTribItemSN[index];

        /// <summary>Quantidade de itens.</summary>
        public int GetGTribItemSNCount => GTribItemSN?.Count ?? 0;
#endif

        internal override void ProcessReader()
        {
            if (XmlReader == null)
            {
                return;
            }
            VersaoEvento = XmlReader.GetAttribute("versaoEvento");
            XElement detalhe;
            using (var reader = XmlReader.ReadSubtree())
            {
                detalhe = XElement.Load(reader);
            }
            XmlReader.Read();
            XNamespace ns = "http://www.portalfiscal.inf.br/nfcom";
            var evento = detalhe.Element(ns + "evTributacaoSN");
            DescEvento = (string)evento.Element(ns + "descEvento");
            NProt = (string)evento.Element(ns + "nProt");
            GTribItemSN = new List<GTribItemSN>();
            foreach (var item in evento.Elements(ns + "gTribItemSN"))
            {
                GTribItemSN.Add(LerGrupo<GTribItemSN>(item));
            }
            GTotalSN = LerGrupo<GTotalSN>(evento.Element(ns + "gTotalSN"));
        }

        private static T LerGrupo<T>(XElement elemento)
        {
            var serializer = new XmlSerializer(typeof(T), new XmlRootAttribute(elemento.Name.LocalName)
            {
                Namespace = elemento.Name.NamespaceName
            });
            using (var reader = elemento.CreateReader())
            {
                return (T)serializer.Deserialize(reader);
            }
        }

        private static void EscreverGrupo<T>(XmlWriter writer, T grupo, string nome)
        {
            if (grupo == null)
            {
                return;
            }
            const string ns = "http://www.portalfiscal.inf.br/nfcom";
            var serializer = new XmlSerializer(typeof(T), new XmlRootAttribute(nome) { Namespace = ns });
            var namespaces = new XmlSerializerNamespaces();
            namespaces.Add(string.Empty, ns);
            serializer.Serialize(writer, grupo, namespaces);
        }

        /// <summary>Escreve o detalhamento do evento no XML.</summary>
        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);
            writer.WriteStartElement("evTributacaoSN", "http://www.portalfiscal.inf.br/nfcom");
            writer.WriteElementString("descEvento", DescEvento);
            writer.WriteElementString("nProt", NProt);
            if (GTribItemSN != null)
            {
                foreach (var item in GTribItemSN)
                {
                    EscreverGrupo(writer, item, "gTribItemSN");
                }
            }
            EscreverGrupo(writer, GTotalSN, "gTotalSN");
            writer.WriteEndElement();
        }
    }

    /// <summary>Evento 412141: cancelamento da tributação do Simples Nacional.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.DetEventoCancTributacaoSN")]
    [ComVisible(true)]
#endif
    public class DetEventoCancTributacaoSN : EventoDetalhe
    {
        /// <summary>Descrição do evento.</summary>
        public override string DescEvento { get; set; } = "Cancelamento da Tributação do Simples Nacional";

        /// <summary>Protocolo de autorização do documento.</summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>Protocolo do evento de tributação a cancelar.</summary>
        [XmlElement("nProtEvTribSN")]
        public string NProtEvTribSN { get; set; }

        internal override void ProcessReader()
        {
            if (XmlReader == null)
            {
                return;
            }
            VersaoEvento = XmlReader.GetAttribute("versaoEvento");
            XElement detalhe;
            using (var reader = XmlReader.ReadSubtree())
            {
                detalhe = XElement.Load(reader);
            }
            XmlReader.Read();
            XNamespace ns = "http://www.portalfiscal.inf.br/nfcom";
            var evento = detalhe.Element(ns + "evCancTribSN");
            DescEvento = (string)evento.Element(ns + "descEvento");
            NProt = (string)evento.Element(ns + "nProt");
            NProtEvTribSN = (string)evento.Element(ns + "nProtEvTribSN");
        }

        /// <summary>Escreve o detalhamento do evento no XML.</summary>
        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);
            writer.WriteStartElement("evCancTribSN", "http://www.portalfiscal.inf.br/nfcom");
            writer.WriteElementString("descEvento", DescEvento);
            writer.WriteElementString("nProt", NProt);
            writer.WriteElementString("nProtEvTribSN", NProtEvTribSN);
            writer.WriteEndElement();
        }
    }

    /// <summary>Tributação do Simples Nacional por item.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GTribItemSN")]
    [ComVisible(true)]
#endif
    public class GTribItemSN
    {
        /// <summary>Número do item.</summary>
        [XmlAttribute("nItem")]
        public string NItem { get; set; }

        /// <summary>Valor de vRBSNItem.</summary>
        [XmlIgnore]
        public double VRBSNItem { get; set; }

        /// <summary>Propriedade auxiliar para serialização de VRBSNItem.</summary>
        [XmlElement("vRBSNItem")]
        public string VRBSNItemField
        {
            get => VRBSNItem.ToString("F2", CultureInfo.InvariantCulture);
            set => VRBSNItem = Converter.ToDouble(value);
        }

        /// <summary>Tipo de receita bruta do Simples Nacional.</summary>
        [XmlElement("tpRBSN")]
        public TipoReceitaBrutaSimplesNacional TpRBSN { get; set; }
        /// <summary>Alíquota de pIBSSN.</summary>
        [XmlIgnore]
#if INTEROP
        public double PIBSSN { get; set; } = -1;
#else
        public double? PIBSSN { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de PIBSSN.</summary>
        [XmlElement("pIBSSN")]
        public string PIBSSNField
        {
            get => ((double?)PIBSSN)?.ToString("0.00##", CultureInfo.InvariantCulture);
            set => PIBSSN = Converter.ToDouble(value);
        }

        /// <summary>Indica se PIBSSN foi informado.</summary>
#if INTEROP
        public bool ShouldSerializePIBSSNField() => PIBSSN >= 0;
#else
        public bool ShouldSerializePIBSSNField() => PIBSSN.HasValue;
#endif

        /// <summary>Valor de vIBSSN.</summary>
        [XmlIgnore]
#if INTEROP
        public double VIBSSN { get; set; } = -1;
#else
        public double? VIBSSN { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de VIBSSN.</summary>
        [XmlElement("vIBSSN")]
        public string VIBSSNField
        {
            get => ((double?)VIBSSN)?.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSSN = Converter.ToDouble(value);
        }

        /// <summary>Indica se VIBSSN foi informado.</summary>
#if INTEROP
        public bool ShouldSerializeVIBSSNField() => VIBSSN >= 0;
#else
        public bool ShouldSerializeVIBSSNField() => VIBSSN.HasValue;
#endif

        /// <summary>Alíquota de pCBSSN.</summary>
        [XmlIgnore]
#if INTEROP
        public double PCBSSN { get; set; } = -1;
#else
        public double? PCBSSN { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de PCBSSN.</summary>
        [XmlElement("pCBSSN")]
        public string PCBSSNField
        {
            get => ((double?)PCBSSN)?.ToString("0.00##", CultureInfo.InvariantCulture);
            set => PCBSSN = Converter.ToDouble(value);
        }

        /// <summary>Indica se PCBSSN foi informado.</summary>
#if INTEROP
        public bool ShouldSerializePCBSSNField() => PCBSSN >= 0;
#else
        public bool ShouldSerializePCBSSNField() => PCBSSN.HasValue;
#endif

        /// <summary>Valor de vCBSSN.</summary>
        [XmlIgnore]
#if INTEROP
        public double VCBSSN { get; set; } = -1;
#else
        public double? VCBSSN { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de VCBSSN.</summary>
        [XmlElement("vCBSSN")]
        public string VCBSSNField
        {
            get => ((double?)VCBSSN)?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBSSN = Converter.ToDouble(value);
        }

        /// <summary>Indica se VCBSSN foi informado.</summary>
#if INTEROP
        public bool ShouldSerializeVCBSSNField() => VCBSSN >= 0;
#else
        public bool ShouldSerializeVCBSSNField() => VCBSSN.HasValue;
#endif

        /// <summary>Valor de vIBSPendSusp.</summary>
        [XmlIgnore]
#if INTEROP
        public double VIBSPendSusp { get; set; } = -1;
#else
        public double? VIBSPendSusp { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de VIBSPendSusp.</summary>
        [XmlElement("vIBSPendSusp")]
        public string VIBSPendSuspField
        {
            get => ((double?)VIBSPendSusp)?.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSPendSusp = Converter.ToDouble(value);
        }

        /// <summary>Indica se VIBSPendSusp foi informado.</summary>
#if INTEROP
        public bool ShouldSerializeVIBSPendSuspField() => VIBSPendSusp >= 0;
#else
        public bool ShouldSerializeVIBSPendSuspField() => VIBSPendSusp.HasValue;
#endif

        /// <summary>Valor de vCBSPendSusp.</summary>
        [XmlIgnore]
#if INTEROP
        public double VCBSPendSusp { get; set; } = -1;
#else
        public double? VCBSPendSusp { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de VCBSPendSusp.</summary>
        [XmlElement("vCBSPendSusp")]
        public string VCBSPendSuspField
        {
            get => ((double?)VCBSPendSusp)?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBSPendSusp = Converter.ToDouble(value);
        }

        /// <summary>Indica se VCBSPendSusp foi informado.</summary>
#if INTEROP
        public bool ShouldSerializeVCBSPendSuspField() => VCBSPendSusp >= 0;
#else
        public bool ShouldSerializeVCBSPendSuspField() => VCBSPendSusp.HasValue;
#endif


    }

    /// <summary>Totais da tributação do Simples Nacional.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GTotalSN")]
    [ComVisible(true)]
#endif
    public class GTotalSN
    {
        /// <summary>Valor de vRBSNTot.</summary>
        [XmlIgnore]
        public double VRBSNTot { get; set; }

        /// <summary>Propriedade auxiliar para serialização de VRBSNTot.</summary>
        [XmlElement("vRBSNTot")]
        public string VRBSNTotField
        {
            get => VRBSNTot.ToString("F2", CultureInfo.InvariantCulture);
            set => VRBSNTot = Converter.ToDouble(value);
        }

        /// <summary>Valor de vIBSSNTot.</summary>
        [XmlIgnore]
#if INTEROP
        public double VIBSSNTot { get; set; } = -1;
#else
        public double? VIBSSNTot { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de VIBSSNTot.</summary>
        [XmlElement("vIBSSNTot")]
        public string VIBSSNTotField
        {
            get => ((double?)VIBSSNTot)?.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSSNTot = Converter.ToDouble(value);
        }

        /// <summary>Indica se VIBSSNTot foi informado.</summary>
#if INTEROP
        public bool ShouldSerializeVIBSSNTotField() => VIBSSNTot >= 0;
#else
        public bool ShouldSerializeVIBSSNTotField() => VIBSSNTot.HasValue;
#endif

        /// <summary>Valor de vIBSSNtotPendSusp.</summary>
        [XmlIgnore]
#if INTEROP
        public double VIBSSNtotPendSusp { get; set; } = -1;
#else
        public double? VIBSSNtotPendSusp { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de VIBSSNtotPendSusp.</summary>
        [XmlElement("vIBSSNtotPendSusp")]
        public string VIBSSNtotPendSuspField
        {
            get => ((double?)VIBSSNtotPendSusp)?.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSSNtotPendSusp = Converter.ToDouble(value);
        }

        /// <summary>Indica se VIBSSNtotPendSusp foi informado.</summary>
#if INTEROP
        public bool ShouldSerializeVIBSSNtotPendSuspField() => VIBSSNtotPendSusp >= 0;
#else
        public bool ShouldSerializeVIBSSNtotPendSuspField() => VIBSSNtotPendSusp.HasValue;
#endif

        /// <summary>Valor de vCBSSNTot.</summary>
        [XmlIgnore]
#if INTEROP
        public double VCBSSNTot { get; set; } = -1;
#else
        public double? VCBSSNTot { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de VCBSSNTot.</summary>
        [XmlElement("vCBSSNTot")]
        public string VCBSSNTotField
        {
            get => ((double?)VCBSSNTot)?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBSSNTot = Converter.ToDouble(value);
        }

        /// <summary>Indica se VCBSSNTot foi informado.</summary>
#if INTEROP
        public bool ShouldSerializeVCBSSNTotField() => VCBSSNTot >= 0;
#else
        public bool ShouldSerializeVCBSSNTotField() => VCBSSNTot.HasValue;
#endif

        /// <summary>Valor de vCBSSNtotPendSusp.</summary>
        [XmlIgnore]
#if INTEROP
        public double VCBSSNtotPendSusp { get; set; } = -1;
#else
        public double? VCBSSNtotPendSusp { get; set; }
#endif

        /// <summary>Propriedade auxiliar para serialização de VCBSSNtotPendSusp.</summary>
        [XmlElement("vCBSSNtotPendSusp")]
        public string VCBSSNtotPendSuspField
        {
            get => ((double?)VCBSSNtotPendSusp)?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBSSNtotPendSusp = Converter.ToDouble(value);
        }

        /// <summary>Indica se VCBSSNtotPendSusp foi informado.</summary>
#if INTEROP
        public bool ShouldSerializeVCBSSNtotPendSuspField() => VCBSSNtotPendSusp >= 0;
#else
        public bool ShouldSerializeVCBSSNtotPendSuspField() => VCBSSNtotPendSusp.HasValue;
#endif


    }
}
