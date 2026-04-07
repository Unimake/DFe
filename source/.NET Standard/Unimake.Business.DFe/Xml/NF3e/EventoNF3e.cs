#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Xml;
using System.Reflection;
using Unimake.Business.DFe.Utility;
using System.Collections.Generic;
using System.Xml.Schema;
using System.Text;
using System.Xml.Linq;

namespace Unimake.Business.DFe.Xml.NF3e
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.EventoNF3e")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nf3e")]
    [XmlRoot("eventoNF3e", Namespace = "http://www.portalfiscal.inf.br/nf3e", IsNullable = false)]
    public class EventoNF3e : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("infEvento")]
        public InfEvento InfEvento { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

        #region Public Methods

        /// <summary>
        /// Desserializar o XML no objeto EventoNF3e
        /// </summary>
        /// <param name="filename">Localização do arquivo XML do EventoNF3e</param>
        /// <returns>Objeto do EventoNF3e</returns>
        public EventoNF3e LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<EventoNF3e>(doc);
        }

        /// <summary>
        /// Desserializar o XML EventoNF3e no objeto EventoNF3e
        /// </summary>
        /// <param name="xml">string do XML EventoNF3e</param>
        /// <returns>Objeto da EventoNF3e</returns>
        public EventoNF3e LoadFromXML(string xml) => XMLUtility.Deserializar<EventoNF3e>(xml);

        #endregion Public Methods

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.InfEvento")]
    [ComVisible(true)]
#endif
    public class InfEvento
    {
        #region Private fields

        private EventoDetalhe _detEvento;

        #endregion Private fields
        
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id
        {
            get => "ID" + ((int)TpEvento).ToString() + ChNF3e + NSeqEvento.ToString("000");
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

        [XmlElement("chNF3e")]
        public string ChNF3e { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhEvento { get; set; }
#else
        public DateTimeOffset DhEvento { get; set; }
#endif

        [XmlElement("dhEvento")]
        public string DhEventoField
        {
            get => DhEvento.ToString("yyyy-MM-ddTHH:mm:sszzz"); //yyyy-MM-ddTHH:mm:sszzz
#if INTEROP
            set => DhEvento = DateTime.Parse(value);
#else
            set => DhEvento = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tpEvento")]
        public TipoEventoNF3e TpEvento { get; set; }

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

                    case TipoEventoNF3e.Cancelamento:
                        _detEvento = new DetEventoCanc();
                        break;

                    case TipoEventoNF3e.VinculacaoPagamento:
                        _detEvento = new DetEventoVincPgto();
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
    [ProgId("Unimake.Business.DFe.Xml.NF3e.EventoDetalhe")]
    [ComVisible(true)]
#endif
    public class EventoDetalhe : IXmlSerializable
    {
        #region Internal Properties

        internal XmlReader XmlReader { get; set; }

        private static readonly BindingFlags bindingFlags = BindingFlags.Public |
            BindingFlags.Instance |
            BindingFlags.IgnoreCase;

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

        internal virtual void SetValue(PropertyInfo pi) =>
            pi?.SetValue(this, Converter.ToAny(XmlReader.GetValue<object>(XmlReader.Name), pi.PropertyType));

        protected internal PropertyInfo GetPropertyInfo(Type type)
        {
            var pi = hasField.Exists(w => w.ToLower() == XmlReader.Name.ToLower()) ? 
                type.GetProperty(XmlReader.Name + "Field", bindingFlags) : type.GetProperty(XmlReader.Name, bindingFlags);

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
    [ProgId("Unimake.Business.DFe.Xml.NF3e.DetEventoCanc")]
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
            <evCancNF3e>
            <descEvento>{DescEvento}</descEvento>
            <nProt>{NProt}</nProt>
            <xJust>{XJust}</xJust>
            </evCancNF3e>");
        }

        #endregion Public Methods
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.DetEventoVincPgto")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Detalhamento do evento de vinculação do pagamento da NF3e.
    /// </summary>
    public class DetEventoVincPgto : EventoDetalhe
    {
        /// <summary>
        /// Versão do leiaute do detalhamento do evento.
        /// </summary>
        [XmlAttribute("versaoEvento", DataType = "string")]
        public override string VersaoEvento { get; set; }

        /// <summary>
        /// Descrição do evento de vinculação do pagamento.
        /// </summary>
        [XmlElement("descEvento")]
        public override string DescEvento { get; set; } = "Vinculacao do Pagamento";

        /// <summary>
        /// Número do protocolo de autorização do DFe.
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Dados da transação de pagamento vinculada.
        /// </summary>
        [XmlElement("pgto")]
        public Pgto Pgto { get; set; }

        /// <summary>
        /// Processa o leitor XML para desserialização do detalhamento de vinculação de pagamento.
        /// </summary>
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
                                    Pgto.TpMeioPgto = (MeioPagamento)int.Parse(child.Value);
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

        /// <summary>
        /// Serializa o detalhamento de vinculação de pagamento no XML do evento.
        /// </summary>
        /// <param name="writer">Escritor XML utilizado na serialização.</param>
        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            writer.WriteRaw($@"
            <evVincPgto>
            <descEvento>{DescEvento}</descEvento>
            <nProt>{NProt}</nProt>
            <pgto nPag=""{Pgto?.NPag}"" idTransacao=""{Pgto?.IdTransacao}"">
            <tpMeioPgto>{(Pgto != null ? ((int)Pgto.TpMeioPgto).ToString("00") : "")}</tpMeioPgto>
            <CNPJReceb>{Pgto?.CNPJReceb}</CNPJReceb>
            <CNPJBasePSP>{Pgto?.CNPJBasePSP}</CNPJBasePSP>
            </pgto>
            </evVincPgto>");
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.Pgto")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Dados do pagamento vinculados ao evento da NF3e.
    /// </summary>
    public class Pgto
    {
        /// <summary>
        /// Número sequencial do pagamento.
        /// </summary>
        [XmlAttribute(AttributeName = "nPag", DataType = "token")]
        public string NPag { get; set; }

        /// <summary>
        /// Identificador da transação do pagamento.
        /// </summary>
        [XmlAttribute(AttributeName = "idTransacao", DataType = "token")]
        public string IdTransacao { get; set; }

        /// <summary>
        /// Meio de pagamento utilizado.
        /// </summary>
        [XmlElement("tpMeioPgto")]
        public MeioPagamento TpMeioPgto { get; set; }

        /// <summary>
        /// CNPJ do recebedor do pagamento.
        /// </summary>
        [XmlElement("CNPJReceb")]
        public string CNPJReceb { get; set; }

        /// <summary>
        /// CNPJ base do provedor de serviço de pagamento.
        /// </summary>
        [XmlElement("CNPJBasePSP")]
        public string CNPJBasePSP { get; set; }
    }
}