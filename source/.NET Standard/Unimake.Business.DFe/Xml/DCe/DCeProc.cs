#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.DCe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.DCeProc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("dceProc", Namespace = "http://www.portalfiscal.inf.br/dce", IsNullable = false)]
    public class DCeProc : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlAttribute(AttributeName = "ipTransmissor", DataType = "token")]
        public string IpTransmissor { get; set; }

        /// <summary>
        /// Porta de origem utilizada na conexão.
        /// </summary>
        [XmlAttribute(AttributeName = "nPortaCon")]
        public int NPortaCon { get; set; }

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

        [XmlElement("DCe")]
        public DCe DCe { get; set; }

        [XmlElement("protDCe")]
        public ProtDCe ProtDCe { get; set; }

        [XmlIgnore]
        public string NomeArquivoDistribuicao => ProtDCe.InfProt.ChDCe + "-procDCe.xml";

        public bool ShouldSerializeIpTransmissor() => !string.IsNullOrEmpty(IpTransmissor);
        /// <summary>
        /// Verifica se a porta de conexão foi informada para serialização.
        /// </summary>
        /// <returns>True quando a porta for maior que zero.</returns>
        public bool ShouldSerializeNPortaCon() => NPortaCon > 0;
        public bool ShouldSerializeDhConexaoField() => DhConexao > DateTime.MinValue;

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();
            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
            ((XmlElement)xmlDocument.GetElementsByTagName("DCe")[0]).SetAttribute("xmlns", attribute.Namespace);
            ((XmlElement)xmlDocument.GetElementsByTagName("protDCe")[0]).SetAttribute("xmlns", attribute.Namespace);
            return xmlDocument;
        }

        public DCeProc LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<DCeProc>(doc);
        }

        public DCeProc LoadFromXML(string xml) => XMLUtility.Deserializar<DCeProc>(xml);
    }
}
