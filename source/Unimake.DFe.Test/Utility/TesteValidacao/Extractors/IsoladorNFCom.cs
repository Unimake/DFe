using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Unimake.DFe.Test.Utility.TesteValidacao.Interfaces;

namespace Unimake.DFe.Test.Utility.TesteValidacao.Extractors
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

            XmlDocument xmlEspecifico = new();
            xmlEspecifico.LoadXml(detEvento.InnerXml);

            return xmlEspecifico;
        }

    }
}
