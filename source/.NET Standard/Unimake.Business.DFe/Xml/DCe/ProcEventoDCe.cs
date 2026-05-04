#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.DCe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.ProcEventoDCe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("procEventoDCe", Namespace = "http://www.portalfiscal.inf.br/dce", IsNullable = false)]
    public class ProcEventoDCe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlAttribute(AttributeName = "ipTransmissor", DataType = "token")]
        public string IpTransmissor { get; set; }

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

        [XmlElement("eventoDCe")]
        public EventoDCe EventoDCe { get; set; }

        [XmlElement("retEventoDCe")]
        public RetEventoDCe RetEventoDCe { get; set; }

        [XmlIgnore]
        public string NomeArquivoDistribuicao => RetEventoDCe.InfEvento.ChDCe + "-procEventoDCe.xml";

        public bool ShouldSerializeIpTransmissor() => !string.IsNullOrEmpty(IpTransmissor);
        public bool ShouldSerializeDhConexaoField() => DhConexao > DateTime.MinValue;
    }
}
