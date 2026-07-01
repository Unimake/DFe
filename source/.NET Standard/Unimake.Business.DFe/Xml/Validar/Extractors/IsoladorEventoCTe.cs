using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Interfaces;

namespace Unimake.Business.DFe.Xml.Validar.Extractors
{
    internal class IsoladorEventoCTe : IXmlEspecificoIsolador
    {
        public XmlDocument Isolar(XmlNode node)
        {
            var xmlEspecifico = new XmlDocument();
            IsolarEvento(node, xmlEspecifico);
            return xmlEspecifico;
        }

        private void IsolarEvento(XmlNode node, XmlDocument xmlEspecifico)
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
