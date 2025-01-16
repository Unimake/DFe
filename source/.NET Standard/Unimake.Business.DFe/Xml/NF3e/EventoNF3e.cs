#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NF3e
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.EventoNF3e")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eventoNF3e", Namespace = "http://www.portalfiscal.inf.br/nf3e", IsNullable = false)]
    public class EventoNF3e : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("infEvento")]
        public InfEvento InfEvento { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.InfEvento")]
    [ComVisible(true)]
#endif
    public class InfEvento
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Versao { get; set; }
        
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
        public string NSeqEvento { get; set; }

        [XmlElement("detEvento")]
        public DetEvento DetEvento {  get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.DetEvento")]
    [ComVisible(true)]
#endif
    public class DetEvento
    {
        [XmlAttribute("versaoEvento", DataType = "string")]
        public string VersaoEvento { get; set; }

        [XmlElement("evCancNF3e", Namespace = "http://www.portalfiscal.inf.br/nf3e")]
        public EvCancNF3e EvCancNF3e { get; set; }
    }
}