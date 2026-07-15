using System.Globalization;
using System.Xml;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    internal sealed class NFeTxtXmlSerializer
    {
        internal XmlDocument Serializar(NFe nota)
        {
            var documento = XMLUtility.Serializar(new Mapping.NFeDFeMapper().Mapear(nota));
            AjustarCompatibilidade(documento);
            return documento;
        }

        private static void AjustarCompatibilidade(XmlDocument documento)
        {
            foreach (XmlElement referencia in documento.SelectNodes("//*[local-name()='gPagAntecipado']/*[local-name()='refDFe']"))
            {
                var compativel = documento.CreateElement("refNFe", referencia.NamespaceURI);
                compativel.InnerText = referencia.InnerText;
                referencia.ParentNode.ReplaceChild(compativel, referencia);
            }
            RenomearElementos(documento, "//*[local-name()='IS']/*[local-name()='adRemIS']", "pISEspec");
            RemoverNaoPositivos(documento, "//*[local-name()='gIBSCBS']/*[local-name()='vIBS']");
        }

        private static void RemoverNaoPositivos(XmlDocument documento, string xpath)
        {
            foreach (XmlElement elemento in documento.SelectNodes(xpath))
            {
                decimal valor;
                if (decimal.TryParse(elemento.InnerText, NumberStyles.Number, CultureInfo.InvariantCulture, out valor) && valor < 0) elemento.ParentNode.RemoveChild(elemento);
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
