using System;
using System.Xml;
using Unimake.DFe.Test.Utility.TesteValidacao.Interfaces;

namespace Unimake.DFe.Test.Utility.TesteValidacao.Isoladores
{
    /// <summary>
    /// Isolador para eventos NFe/NFCe.
    /// Extrai a tag detEvento para validação específica.
    /// </summary>
    internal class IsoladorNFe : IXmlEspecificoIsolador
    {
        public XmlDocument Isolar(XmlNode node)
        {
            var infEvento = node.SelectSingleNode("*[local-name()='infEvento']");
            if (infEvento is null)
                throw new Exception("Tag 'infEvento' não encontrada");

            var detEvento = infEvento.SelectSingleNode("*[local-name()='detEvento']");
            if (detEvento is null)
                throw new Exception("Tag 'detEvento' não encontrada");

            XmlDocument xmlEspecifico = new();
            xmlEspecifico.LoadXml(detEvento.OuterXml);

            return xmlEspecifico;
        }
    }
}