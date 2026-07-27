using System.Globalization;
using System.Collections.Generic;
using System.Xml;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Aplica ajustes pontuais necessários para manter o XML TXT compatível com a saída de referência.
    /// </summary>
    internal static class NFeTxtXmlCompatibilityAdjuster
    {
        internal static void Ajustar(XmlDocument documento, List<Vol> volumes)
        {
            RemoverNegativos(documento, "//*[local-name()='gIBSCBS']/*[local-name()='vIBS']");
            RemoverElementos(documento, "//*[local-name()='IPI' and not(*[local-name()='IPINT']/*[local-name()='CST']) and not(*[local-name()='IPITrib']/*[local-name()='CST'])]");
            if (volumes != null && volumes.Count > 0)
            {
                AdicionarVolumesOmitidos(documento, volumes);
            }
        }

        private static void AdicionarVolumesOmitidos(XmlDocument documento, List<Vol> volumes)
        {
            var transporte = documento.SelectSingleNode("//*[local-name()='transp']") as XmlElement;
            if (transporte == null || transporte.SelectSingleNode("*[local-name()='vol']") != null)
            {
                return;
            }

            foreach (var volume in volumes)
            {
                var elementoVolume = documento.CreateElement("vol", transporte.NamespaceURI);
                transporte.AppendChild(elementoVolume);
                if (volume.Lacres == null)
                {
                    continue;
                }

                foreach (var lacre in volume.Lacres)
                {
                    var elementoLacre = documento.CreateElement("lacres", transporte.NamespaceURI);
                    var numeroLacre = documento.CreateElement("nLacre", transporte.NamespaceURI);
                    numeroLacre.InnerText = lacre.NLacre;
                    elementoLacre.AppendChild(numeroLacre);
                    elementoVolume.AppendChild(elementoLacre);
                }
            }
        }

        private static void RemoverElementos(XmlDocument documento, string xpath)
        {
            foreach (XmlElement elemento in documento.SelectNodes(xpath))
            {
                elemento.ParentNode.RemoveChild(elemento);
            }
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
