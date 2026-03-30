using System.IO;
using System.Xml;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.ConsumirServico.Parsers
{
    internal sealed class ApiResponseXmlSupport
    {
        public XmlDocument StringToSerializedXml(string value)
        {
            var document = new XmlDocument();
            document.LoadXml(StringToXml(value));
            return document;
        }

        public XmlDocument CreatePdfXmlDocument(string text)
        {
            var xmlDoc = new XmlDocument();
            var root = xmlDoc.CreateElement("root");
            xmlDoc.AppendChild(root);

            var textoElement = xmlDoc.CreateElement("Base64Pdf");
            textoElement.InnerText = text;
            root.AppendChild(textoElement);

            return xmlDoc;
        }

        private string StringToXml(string str)
        {
            using (var retorno = new StringWriter())
            {
                var xml = new XmlSerializer(str.GetType());
                xml.Serialize(retorno, str);
                return retorno.ToString();
            }
        }
    }
}
