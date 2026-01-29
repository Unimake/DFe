using System.Collections.Generic;
using System.Xml.Linq;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Validator.Contract
{
    /// <summary>
    /// Faz a validação do XML antes de enviar para o serviço correspondente
    /// </summary>
    public interface IXmlValidator
    {
        /// <summary>
        /// Xml associado ao validador
        /// </summary>
        string Xml { get; set; }

        /// <summary>
        /// Retorna verdadeiro se o validador pode validar o xml informado em <see cref="Xml"/>
        /// </summary>
        /// <param name="element">Elemento que contem o xml que será validado</param>
        /// <returns></returns>
        bool CanValidate(XElement element);

        /// <summary>
        /// Validar o XML
        /// </summary>
        /// <returns>Retorna verdadeiro se tudo OK. Pode lançar erros de validação</returns>
        bool Validate();

        /// <summary>
        /// Warnings gerados durante a validação do XML
        /// </summary>
        List<ValidatorDFeException> Warnings { get; set; }
    }
}