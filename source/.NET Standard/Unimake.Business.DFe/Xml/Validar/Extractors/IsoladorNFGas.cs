using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Interfaces;

namespace Unimake.Business.DFe.Xml.Validar.Extractors
{
    internal class IsoladorNFGas : IXmlEspecificoIsolador
    {
        public XmlDocument Isolar(XmlNode node) 
        {
            var infEvento = node.SelectSingleNode("*[local-name()='infEvento']");
            if (infEvento is null)
                throw new Exception("Tag 'infEvento' não encontrada");

            var detEvento = infEvento.SelectSingleNode("*[local-name()='detEvento']");
            if (detEvento is null)
                throw new Exception("Tag 'detEvento' não encontrada");

            XmlDocument xmlEspecifico = new XmlDocument();
            xmlEspecifico.LoadXml(detEvento.InnerXml);

            return xmlEspecifico;
        }
    }
}
