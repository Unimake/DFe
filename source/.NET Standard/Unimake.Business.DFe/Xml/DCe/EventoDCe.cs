#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.DCe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.EventoDCe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eventoDCe", Namespace = "http://www.portalfiscal.inf.br/dce", IsNullable = false)]
    public class EventoDCe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("infEvento")]
        public InfEvento InfEvento { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

        public EventoDCe LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<EventoDCe>(doc);
        }

        public EventoDCe LoadFromXML(string xml) => XMLUtility.Deserializar<EventoDCe>(xml);
    }

    public class InfEvento
    {
        private EventoDetalhe detEvento;

        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id
        {
            get => "ID" + ((int)TpEvento).ToString() + ChDCe + NSeqEvento.ToString("000");
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

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("chDCe")]
        public string ChDCe { get; set; }

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
        public TipoEventoDCe TpEvento { get; set; }

        [XmlElement("nSeqEvento")]
        public int NSeqEvento { get; set; }

        [XmlElement("detEvento")]
        public EventoDetalhe DetEvento
        {
            get => detEvento;
            set
            {
                switch (TpEvento)
                {
                    case 0:
                        detEvento = value;
                        break;

                    case TipoEventoDCe.Cancelamento:
                        detEvento = new DetEventoCanc();
                        break;

                    default:
                        throw new NotImplementedException($"O tipo de evento '{TpEvento}' não está implementado.");
                }

                detEvento.XmlReader = value.XmlReader;
                detEvento.XmlString = value.XmlString;
                detEvento.ProcessReader();
            }
        }

        public InfEvento() { }

        public InfEvento(EventoDetalhe detEvento) => DetEvento = detEvento ?? throw new ArgumentNullException(nameof(detEvento));

        public bool ShouldSerializeCNPJ() => !string.IsNullOrEmpty(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrEmpty(CPF);
    }

    public class EventoDetalhe : IXmlSerializable
    {
        internal XmlReader XmlReader { get; set; }
        internal string XmlString { get; set; }

        internal virtual void ProcessReader()
        {
            if (!string.IsNullOrWhiteSpace(XmlString))
            {
                var doc = new XmlDocument();
                doc.LoadXml(XmlString);

                VersaoEvento = doc.DocumentElement?.GetAttribute("versaoEvento");

                foreach (XmlNode node in doc.DocumentElement.SelectNodes(".//*"))
                {
                    var pi = GetType().GetProperty(node.Name, BindingFlags.Public | BindingFlags.Instance | BindingFlags.IgnoreCase);
                    pi?.SetValue(this, Converter.ToAny(node.InnerText, pi.PropertyType));
                }

                return;
            }

            if (XmlReader == null)
            {
                return;
            }

            var type = GetType();

            if (XmlReader.HasAttributes && XmlReader.GetAttribute("versaoEvento") != "")
            {
                var pi = type.GetProperty("versaoEvento", BindingFlags.Public | BindingFlags.Instance | BindingFlags.IgnoreCase);
                pi?.SetValue(this, XmlReader.GetAttribute("versaoEvento"));
            }

            while (XmlReader.Read())
            {
                if (XmlReader.NodeType == XmlNodeType.EndElement && XmlReader.Name == "detEvento")
                {
                    break;
                }

                if (XmlReader.NodeType != XmlNodeType.Element)
                {
                    continue;
                }

                var pi = type.GetProperty(XmlReader.Name, BindingFlags.Public | BindingFlags.Instance | BindingFlags.IgnoreCase);
                pi?.SetValue(this, Converter.ToAny(XmlReader.GetValue<object>(XmlReader.Name), pi.PropertyType));
            }
        }

        [XmlElement("descEvento")]
        public virtual string DescEvento { get; set; }

        [XmlAttribute(AttributeName = "versaoEvento", DataType = "token")]
        public virtual string VersaoEvento { get; set; }

        public XmlSchema GetSchema() => default;
        public void ReadXml(XmlReader reader) => XmlString = reader.ReadOuterXml();
        public virtual void WriteXml(XmlWriter writer) => writer.WriteAttributeString("versaoEvento", VersaoEvento);
    }

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

        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);

            writer.WriteRaw($@"
            <evCancDCe>
            <descEvento>{DescEvento}</descEvento>
            <nProt>{NProt}</nProt>
            <xJust>{XJust}</xJust>
            </evCancDCe>");
        }
    }
}
