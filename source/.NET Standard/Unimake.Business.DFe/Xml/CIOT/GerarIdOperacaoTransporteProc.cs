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

namespace Unimake.Business.DFe.Xml.CIOT
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.GerarIdOperacaoTransporteProc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("GerarIdOperacaoTransporteProc", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class GerarIdOperacaoTransporteProc : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("GerarIdOperacaoTransporte")]
        public GerarIdOperacaoTransporte GerarIdOperacaoTransporte { get; set; }

        [XmlElement("RetGerarIdOperacaoTransporte")]
        public RetGerarIdOperacaoTransporte RetGerarIdOperacaoTransporte { get; set; }

        [XmlIgnore]
        public string NomeArquivoDistribuicao => RetGerarIdOperacaoTransporte.IdOperacaoTransporte + "-procIdOpTransp.xml";

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();
            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
            ((XmlElement)xmlDocument.GetElementsByTagName("GerarIdOperacaoTransporte")[0]).SetAttribute("xmlns", attribute.Namespace);
            ((XmlElement)xmlDocument.GetElementsByTagName("RetGerarIdOperacaoTransporte")[0]).SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        public GerarIdOperacaoTransporteProc LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<GerarIdOperacaoTransporteProc>(doc);
        }

        public GerarIdOperacaoTransporteProc LoadFromXML(string xml) => XMLUtility.Deserializar<GerarIdOperacaoTransporteProc>(xml);
    }
}
