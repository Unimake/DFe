using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Interfaces;

namespace Unimake.Business.DFe.Xml.Validar.Extractors
{
    internal class IsoladorESocial : IXmlEspecificoIsolador
    {
        public XmlDocument Isolar(XmlNode node) 
        {
            var eSocialEvento = node.SelectSingleNode("*[local-name()='eSocial']");

            if (eSocialEvento is null)
            {
                throw new Exception("Não foi encontrado o node eSocial no evento do lote");
            }

            XmlDocument xmlEspecifico = new XmlDocument();
            xmlEspecifico.LoadXml(eSocialEvento.OuterXml);
            return xmlEspecifico;
        }
    }
}
