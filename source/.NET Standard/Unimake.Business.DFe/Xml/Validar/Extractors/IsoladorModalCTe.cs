using System;
using System.Xml;
using Unimake.Business.DFe.Interfaces;

namespace Unimake.Business.DFe.Isoladores
{
    /// <summary>
    /// Isolador para CTe (modais e eventos).
    /// Extrai infModal ou detEvento conforme necessário.
    /// </summary>
    internal class IsoladorModalCTe : IXmlEspecificoIsolador
    {
        public XmlDocument Isolar(XmlNode node)
        {
            var xmlEspecifico = new XmlDocument();
            var infModal = node.SelectSingleNode(".//*[local-name()='infModal']");

            // Alguns CT-es (ex.: complemento) não possuem infModal.
            // Nestes casos não existe XML específico para validar.
            if (infModal is null)
            {
                return null;
            }

            xmlEspecifico.LoadXml(infModal.InnerXml);
            return xmlEspecifico;
        }
    }
}