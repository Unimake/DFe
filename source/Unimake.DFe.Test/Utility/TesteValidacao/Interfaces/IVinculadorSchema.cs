using System;
using System.Collections.Generic;

using System.Xml;

namespace Unimake.DFe.Test.Utility.TesteValidacao.Interfaces
{
    /// <summary>
    /// Interface que define o contrato para vinculadores de schema específicos para cada tipo de DFe.
    /// Cada vinculador implementa sua própria lógica de extração de modal/evento e vinculação ao schema.
    /// </summary>
    internal interface IVinculadorSchema
    {
        /// <summary>
        /// Vincula nós de modal ou evento ao seu schema específico correspondente.
        /// </summary>
        /// <param name="servico">Nó de serviço da configuração XML</param>
        /// <param name="xml">Documento XML a ser validado</param>
        /// <returns>Lista de tuplas contendo (TipoSchema, Node) vinculados</returns>
        /// <exception cref="Exception">Quando configuração ou estrutura XML é inválida</exception>
        List<(XmlNode TipoSchema, XmlNode Node)> Vincular(XmlNode servico, XmlDocument xml);
    }
}