#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
# endif

using System;
using System.Xml.Serialization;
using System.Xml;

namespace Unimake.Business.DFe.Xml.NF3e
{

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ProcEventoNF3e")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("procEventoNF3e", Namespace = "http://www.portalfiscal.inf.br/nf3e", IsNullable = false)]
    public class ProcEventoNF3e : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlAttribute(AttributeName = "ipTransmissor", DataType = "token")]
        public string IpTransmissor { get; set; }

        [XmlAttribute(AttributeName = "nPortaCon", DataType = "token")]
        public string NPortaCon { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhConexao { get; set; }
#else
        public DateTimeOffset DhConexao { get; set; }
#endif

        [XmlAttribute(AttributeName = "dhConexao", DataType = "token")]
        public string DhConexaoField
        {
            get => DhConexao.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhConexao = DateTime.Parse(value);
#else
            set => DhConexao = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("eventoNF3e", Namespace = "http://www.portalfiscal.inf.br/nf3e")]
        public string EventoNF3e { get; set; }

        [XmlElement("retEventoNF3e", Namespace = "http://www.portalfiscal.inf.br/nf3e")]
        public string RetEventoNF3e { get; set; }
    }
}