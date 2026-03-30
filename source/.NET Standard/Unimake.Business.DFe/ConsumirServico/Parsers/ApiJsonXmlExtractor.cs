using Newtonsoft.Json;
using System.Xml;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.ConsumirServico.Parsers
{
    internal sealed class ApiJsonXmlExtractor
    {
        public string ExtractXml(ref APIConfig config, string content)
        {
            string result;
            var xml = JsonConvert.DeserializeXmlNode(content, "temp");
            XmlNode node = null;

            try
            {
                var nodes = xml.GetElementsByTagName(config.TagRetorno);
                if (nodes.Count > 0)
                {
                    node = nodes[0];
                    if (node != null && config.TagRetorno != "chaveAcesso" && !string.IsNullOrWhiteSpace(node.InnerXml))
                    {
                        var temp = Compress.GZIPDecompress(node.InnerText);
                        config.TagRetorno = "prop:innertext";
                        return temp;
                    }
                }
            }
            finally
            {
                result = xml.OuterXml;
            }

            return result;
        }
    }
}
