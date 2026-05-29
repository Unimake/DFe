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
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetificacaoOperacaoTransporteProc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("RetificacaoOperacaoTransporteProc", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetificacaoOperacaoTransporteProc : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("RetificacaoOperacaoTransporte")]
        public RetificacaoOperacaoTransporte RetificacaoOperacaoTransporte { get; set; }

        [XmlElement("RetRetificacaoOperacaoTransporte")]
        public RetRetificacaoOperacaoTransporte RetRetificacaoOperacaoTransporte { get; set; }

        [XmlIgnore]
        public string NomeArquivoDistribuicao => RetRetificacaoOperacaoTransporte.CodigoIdentificacaoOperacao + "-procCIOT.xml";

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();
            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
            ((XmlElement)xmlDocument.GetElementsByTagName("RetificacaoOperacaoTransporte")[0]).SetAttribute("xmlns", attribute.Namespace);
            ((XmlElement)xmlDocument.GetElementsByTagName("RetRetificacaoOperacaoTransporte")[0]).SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        public RetificacaoOperacaoTransporteProc LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<RetificacaoOperacaoTransporteProc>(doc);
        }

        public RetificacaoOperacaoTransporteProc LoadFromXML(string xml) => XMLUtility.Deserializar<RetificacaoOperacaoTransporteProc>(xml);
    }
}
