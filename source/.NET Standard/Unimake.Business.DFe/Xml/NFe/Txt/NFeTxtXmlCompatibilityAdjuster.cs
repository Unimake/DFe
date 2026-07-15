using System.Globalization;
using System.Xml;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Aplica ajustes pontuais necessários para manter o XML TXT compatível com a saída de referência.
    /// </summary>
    internal static class NFeTxtXmlCompatibilityAdjuster
    {
        internal static void Ajustar(XmlDocument documento)
        {
            RenomearElementos(documento, "//*[local-name()='gPagAntecipado']/*[local-name()='refDFe']", "refNFe");
            RenomearElementos(documento, "//*[local-name()='IS']/*[local-name()='adRemIS']", "pISEspec");
            RemoverNegativos(documento, "//*[local-name()='gIBSCBS']/*[local-name()='vIBS']");
        }

        private static void RemoverNegativos(XmlDocument documento, string xpath)
        {
            foreach (XmlElement elemento in documento.SelectNodes(xpath))
            {
                decimal valor;
                if (decimal.TryParse(elemento.InnerText, NumberStyles.Number, CultureInfo.InvariantCulture, out valor) && valor < 0)
                {
                    elemento.ParentNode.RemoveChild(elemento);
                }
            }
        }

        private static void RenomearElementos(XmlDocument documento, string xpath, string nome)
        {
            foreach (XmlElement elemento in documento.SelectNodes(xpath))
            {
                var novo = documento.CreateElement(nome, elemento.NamespaceURI);
                novo.InnerText = elemento.InnerText;
                elemento.ParentNode.ReplaceChild(novo, elemento);
            }
        }
    }
}
