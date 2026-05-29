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
    [ProgId("Unimake.Business.DFe.Xml.CIOT.EncerramentoOperacaoTransporteProc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("EncerramentoOperacaoTransporteProc", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class EncerramentoOperacaoTransporteProc : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("EncerramentoOperacaoTransporte")]
        public EncerramentoOperacaoTransporte EncerramentoOperacaoTransporte { get; set; }

        [XmlElement("RetEncerramentoOperacaoTransporte")]
        public RetEncerramentoOperacaoTransporte RetEncerramentoOperacaoTransporte { get; set; }

        [XmlIgnore]
        public string NomeArquivoDistribuicao => RetEncerramentoOperacaoTransporte.CodigoIdentificacaoOperacao + "-procCIOT.xml";

        public override XmlDocument GerarXML()
        {
            var xmlDocument = base.GerarXML();
            var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
            ((XmlElement)xmlDocument.GetElementsByTagName("EncerramentoOperacaoTransporte")[0]).SetAttribute("xmlns", attribute.Namespace);
            ((XmlElement)xmlDocument.GetElementsByTagName("RetEncerramentoOperacaoTransporte")[0]).SetAttribute("xmlns", attribute.Namespace);

            return xmlDocument;
        }

        public EncerramentoOperacaoTransporteProc LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<EncerramentoOperacaoTransporteProc>(doc);
        }

        public EncerramentoOperacaoTransporteProc LoadFromXML(string xml) => XMLUtility.Deserializar<EncerramentoOperacaoTransporteProc>(xml);
    }
}
