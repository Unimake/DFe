using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Interfaces;

namespace Unimake.Business.DFe.Xml.Validar.Extractors
{
    internal class IsoladorEFDReinf : IXmlEspecificoIsolador
    {

        public XmlDocument Isolar(XmlNode node)
        {
            var reinfEvento = node.SelectSingleNode("*[local-name()='Reinf']");

            if (reinfEvento is null)
            {
                throw new Exception("Não foi encontrado o node Reinf no evento do lote");
            }

            XmlDocument xmlEspecifico = new XmlDocument();
            xmlEspecifico.LoadXml(reinfEvento.OuterXml);

            return xmlEspecifico;
        }
    }
}
