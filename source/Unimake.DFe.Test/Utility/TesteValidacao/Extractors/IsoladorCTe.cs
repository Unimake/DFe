using System;
using System.Xml;
using Unimake.DFe.Test.Utility.TesteValidacao.Interfaces;

namespace Unimake.DFe.Test.Utility.TesteValidacao.Isoladores
{
    /// <summary>
    /// Isolador para CTe (modais e eventos).
    /// Extrai infModal ou detEvento conforme necessário.
    /// </summary>
    internal class IsoladorCTe : IXmlEspecificoIsolador
    {
        public XmlDocument Isolar(XmlNode node)
        {
            var xmlEspecifico = new XmlDocument();
            var infModal = node.SelectSingleNode(".//*[local-name()='infModal']");

            if (infModal is null)
            {
                IsoladorEventoCTe(node, xmlEspecifico);
            }
            else
            {
                xmlEspecifico.LoadXml(infModal.InnerXml);
            }

            return xmlEspecifico;
        }

        private void IsoladorEventoCTe(XmlNode node, XmlDocument xmlEspecifico)
        {
            var elementInfEvento = (XmlElement)node.SelectSingleNode("*[local-name()='infEvento']");
            if (elementInfEvento is null)
                throw new Exception("Tag 'infEvento' não encontrada em evento CTe");

            var detEventoCTe = elementInfEvento.SelectSingleNode("*[local-name()='detEvento']");
            if (detEventoCTe is null)
                throw new Exception("Tag 'detEvento' não encontrada em infEvento");

            var detElement = elementInfEvento.GetElementsByTagName(detEventoCTe.FirstChild?.Name)[0];
            if (detElement is null)
                throw new Exception("Elemento específico não encontrado em detEvento");

            xmlEspecifico.LoadXml(detElement.OuterXml);
        }
    }
}