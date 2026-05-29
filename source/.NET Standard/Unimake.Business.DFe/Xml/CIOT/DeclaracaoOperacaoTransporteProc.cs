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
    [ProgId("Unimake.Business.DFe.Xml.CIOT.DeclaracaoOperacaoTransporteProc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("DeclaracaoOperacaoTransporteProc", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]

    public class DeclaracaoOperacaoTransporteProc : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("DeclaracaoOperacaoTransporte")]
        public DeclaracaoOperacaoTransporte DeclaracaoOperacaoTransporte { get; set; }

        [XmlElement("RetDeclaracaoOperacaoTransporte")]
        public RetDeclaracaoOperacaoTransporte RetDeclaracaoOperacaoTransporte { get; set; }

        [XmlIgnore]
        public string NomeArquivoDistribuicao => RetDeclaracaoOperacaoTransporte.IdOperacaoTransporte + "-procCIOT.xml";

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();
            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
            ((XmlElement)xmlDocument.GetElementsByTagName("DeclaracaoOperacaoTransporte")[0]).SetAttribute("xmlns", attribute.Namespace);
            ((XmlElement)xmlDocument.GetElementsByTagName("RetDeclaracaoOperacaoTransporte")[0]).SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        public DeclaracaoOperacaoTransporteProc LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<DeclaracaoOperacaoTransporteProc>(doc);
        }

        public DeclaracaoOperacaoTransporteProc LoadFromXML(string xml) => XMLUtility.Deserializar<DeclaracaoOperacaoTransporteProc>(xml);
    }
}
