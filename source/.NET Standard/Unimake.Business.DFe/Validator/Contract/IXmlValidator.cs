using System.Xml.Linq;

namespace Unimake.Business.DFe.Validator.Contract
{
    /// <summary>
    /// Faz a validação do XML antes de enviar para o serviço correspondente
    /// </summary>
    public interface IXmlValidator
    {
        #region Public Properties

        /// <summary>
        /// Xml associado ao validador
        /// </summary>
        string Xml { get; set; }

        #endregion Public Properties

        #region Public Methods

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

        #endregion Public Methods
    }
}