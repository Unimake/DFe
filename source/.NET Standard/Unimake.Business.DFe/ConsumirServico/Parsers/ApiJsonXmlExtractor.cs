using Newtonsoft.Json;
using System.Xml;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.ConsumirServico.Parsers
{
    internal sealed class ApiJsonXmlExtractor
    {
        private static readonly string[] ElementosComprimidosPadrao = { "ArquivoXml" };

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

                DescompactarElementos(xml, ElementosComprimidosPadrao);
            }
            finally
            {
                result = xml.OuterXml;
            }

            return result;
        }

        private void DescompactarElementos(XmlDocument xml, string[] elementosComprimidos)
        {
            if (xml?.DocumentElement == null || elementosComprimidos == null)
            {
                return;
            }

            foreach (var nomeElemento in elementosComprimidos)
            {
                var nodes = xml.GetElementsByTagName(nomeElemento);

                foreach (XmlElement node in nodes)
                {
                    if (string.IsNullOrWhiteSpace(node.InnerText))
                    {
                        continue;
                    }

                    string conteudoDescompactado;
                    try
                    {
                        conteudoDescompactado = Compress.GZIPDecompress(node.InnerText);
                    }
                    catch
                    {
                        continue; // Não é GZIP válido, mantém original
                    }

                    if (string.IsNullOrWhiteSpace(conteudoDescompactado))
                    {
                        continue;
                    }

                    if (!TentarSubstituirPorXml(node, conteudoDescompactado))
                    {
                        node.RemoveAll();
                        node.AppendChild(node.OwnerDocument.CreateCDataSection(conteudoDescompactado));
                    }
                }
            }
        }

        private bool TentarSubstituirPorXml(XmlElement destino, string conteudo)
        {
            try
            {
                var xmlInterno = new XmlDocument();
                xmlInterno.LoadXml(conteudo);

                destino.RemoveAll();
                var importado = destino.OwnerDocument.ImportNode(xmlInterno.DocumentElement, true);
                destino.AppendChild(importado);

                return true;
            }
            catch
            {
                return false; // Conteúdo não é XML bem‑formado
            }
        }
    }
}
