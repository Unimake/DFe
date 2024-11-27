using System.Text.RegularExpressions;
using System.Xml;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// Classe de utilitários para o eSocial
    /// </summary>
    public static class Utility
    {
        /// <summary>
        /// Substitui o número da versão de schema no XML para a versão que está sendo utilizada
        /// </summary>
        /// <param name="doc">XML</param>
        /// <param name="versaoSchema">Versão do schema utilizado</param>
        /// <returns>XML com a versão já substituída</returns>
        public static XmlDocument ReplaceVersionSchema(XmlDocument doc, string versaoSchema)
        {
            var pattern = @"v_S_\d{2}_\d{2}_\d{2}"; // Expressão para identificar o formato v_S_??_??_??

            var result = Regex.Replace(doc.OuterXml, pattern, versaoSchema);
            var xml = new XmlDocument();
            xml.LoadXml(result);

            return xml;
        }
}
}
