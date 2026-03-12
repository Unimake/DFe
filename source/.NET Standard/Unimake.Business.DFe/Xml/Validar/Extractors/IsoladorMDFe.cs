using System;
using System.Xml;
using Unimake.Business.DFe.Interfaces;

namespace Unimake.Business.DFe.Isoladores
{
    internal class IsoladorMDFe : IXmlEspecificoIsolador
    {
        public XmlDocument Isolar(XmlNode node)
        {
            var xmlEspecifico = new XmlDocument();
            var infModal = node.SelectSingleNode(".//*[local-name()='infModal']");
            if (infModal is null)
            {
                IsoladorEventoMDFe(node, xmlEspecifico);
            }
            else
            {
                xmlEspecifico.LoadXml(infModal.InnerXml);
            }
            return xmlEspecifico;
        }


        private void IsoladorEventoMDFe(XmlNode node, XmlDocument xmlEspecifico)
        {
            var elementInfEvento = (XmlElement)node.SelectSingleNode("*[local-name()='infEvento']");
            if (elementInfEvento is null)
                throw new Exception("Tag 'infEvento' não encontrada em evento MDFe");

            var detEventoMDFe = elementInfEvento.SelectSingleNode("*[local-name()='detEvento']");
            if (detEventoMDFe is null)
                throw new Exception("Tag 'detEvento' não encontrada em infEvento");

            var detElement = elementInfEvento.GetElementsByTagName(detEventoMDFe.FirstChild?.Name)[0];
            if (detElement is null)
                throw new Exception($"Elemento específico não encontrado em detEvento");

            xmlEspecifico.LoadXml(detElement.OuterXml);

        }

    }
}
