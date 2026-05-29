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
    [ProgId("Unimake.Business.DFe.Xml.CIOT.CancelamentoOperacaoTransporteProc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("CancelamentoOperacaoTransporteProc", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class CancelamentoOperacaoTransporteProc : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("CancelamentoOperacaoTransporte")]
        public CancelamentoOperacaoTransporte CancelamentoOperacaoTransporte { get; set; }

        [XmlElement("RetCancelamentoOperacaoTransporte")]
        public RetCancelamentoOperacaoTransporte RetCancelamentoOperacaoTransporte { get; set; }

        [XmlIgnore]
        public string NomeArquivoDistribuicao => RetCancelamentoOperacaoTransporte.CodigoIdentificacaoOperacao + "-procEventoCIOT.xml";

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();
            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
            ((XmlElement)xmlDocument.GetElementsByTagName("CancelamentoOperacaoTransporte")[0]).SetAttribute("xmlns", attribute.Namespace);
            ((XmlElement)xmlDocument.GetElementsByTagName("RetCancelamentoOperacaoTransporte")[0]).SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        public CancelamentoOperacaoTransporteProc LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<CancelamentoOperacaoTransporteProc>(doc);
        }

        public CancelamentoOperacaoTransporteProc LoadFromXML(string xml) => XMLUtility.Deserializar<CancelamentoOperacaoTransporteProc>(xml);
    }
}
