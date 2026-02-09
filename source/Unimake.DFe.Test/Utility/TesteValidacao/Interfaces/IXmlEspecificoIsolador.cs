using System.Xml;

namespace Unimake.DFe.Test.Utility.TesteValidacao.Interfaces
{
    /// <summary>
    /// Interface que define o contrato para isoladores XML específicos de cada tipo de DFe.
    /// Cada isolador implementa sua própria lógica de extração de XML específico.
    /// </summary>
    internal interface IXmlEspecificoIsolador
    {
        /// <summary>
        /// Isola o XML específico (schema) de um nó de evento ou modal.
        /// </summary>
        /// <param name="node">Nó XML contendo evento ou modal</param>
        /// <returns>XmlDocument contendo apenas o XML específico para validação</returns>
        XmlDocument Isolar(XmlNode node);
    }
}