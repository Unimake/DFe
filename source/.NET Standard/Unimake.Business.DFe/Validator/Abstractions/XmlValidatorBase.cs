using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml.Linq;
using Unimake.Business.DFe.Validator.Contract;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Validator.Abstractions
{
    /// <summary>
    /// Faz a validação do XML antes de enviar para o serviço correspondente
    /// </summary>
    public abstract class XmlValidatorBase : IXmlValidator
    {
        private readonly HashSet<(Func<XElement, bool> Predicate, Action<XElement> Validate)> validators = new HashSet<(Func<XElement, bool> Predicate, Action<XElement> Validate)>();

        private Action<XElement> FindMap(XElement element) =>
                    validators.FirstOrDefault(w => w.Predicate(element)).Validate;

        private bool Validate(IEnumerable<XElement> elements)
        {
            //faz um loop pelas tags do XML
            foreach (var tag in elements)
            {
                var validate = FindMap(tag);
                validate?.Invoke(tag);
            }

            return true;
        }

        /// <summary>
        /// <inheritdoc cref="IXmlValidator.Xml"/>
        /// </summary>
        public string Xml { get; set; }

        /// <summary>
        /// Exceções que não interrompem o fluxo do sistema, sendo registradas apenas como avisos ou alertas.
        /// </summary>
        public List<ValidatorDFeException> Warnings
        {
            get => _warnings;
            set => _warnings = value;
        }
        private List<ValidatorDFeException> _warnings = new List<ValidatorDFeException>();

        /// <summary>
        /// Inicia o objeto de validação
        /// </summary>
        public XmlValidatorBase() { }

        /// <summary>
        /// <inheritdoc cref="IXmlValidator.CanValidate(XElement)"/>
        /// </summary>
        /// <param name="element"><inheritdoc cref="IXmlValidator.CanValidate(XElement)"/></param>
        /// <returns></returns>
        public abstract bool CanValidate(XElement element);

        /// <summary>
        /// <inheritdoc cref="IXmlValidator.Validate"/>
        /// </summary>
        public virtual bool Validate()
        {
            //faz um loop pelas tags do XML
            var elements = XDocument.Parse(Xml)
                                    .Descendants()
                                    .Where(element => validators.Any(w => w.Predicate(element)));
            return Validate(elements);
        }

        /// <summary>
        /// Mapeia a tag que será validada no XML
        /// </summary>
        /// <param name="predicate">Condição para realizar a validação na TAG</param>
        /// <param name="validate">Ação que será chamada para validação da tag</param>
        public XmlValidatorBase ValidateTag(Func<XElement, bool> predicate, Action<XElement> validate)
        {
            _ = validators.Add((predicate, validate));
            return this;
        }
    }
}