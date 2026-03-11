using System;
using System.Xml;
using Unimake.Business.DFe.Interfaces;

namespace Unimake.Business.DFe.Isoladores
{
    internal class IsoladorNFCom : IXmlEspecificoIsolador
    {
        public XmlDocument Isolar(XmlNode node) 
        {
            var infEvento = node.SelectSingleNode("*[local-name()='infEvento']");
            if (infEvento is null)
                throw new Exception("tag 'infEvento' não encontrada");

            var detEvento = infEvento.SelectSingleNode("*[local-name()='detEvento']");
            if (detEvento is null)
                throw new Exception("Tag 'detEvento' não encontrada");

            XmlDocument xmlEspecifico = new XmlDocument();
            xmlEspecifico.LoadXml(detEvento.InnerXml);

            return xmlEspecifico;
        }

    }
}
