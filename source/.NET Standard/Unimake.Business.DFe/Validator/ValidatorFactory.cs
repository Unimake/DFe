using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml;
using System.Xml.Linq;
using Unimake.Business.DFe.Validator.Contract;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Validator
{
    /// <summary>
    /// Constrói
    /// </summary>
    public class ValidatorFactory
    {
        #region Private Fields

        private static Dictionary<string, IXmlValidator> xmlsValidators;

        #endregion Private Fields

        #region Private Constructors

        private ValidatorFactory()
        {
        }

        #endregion Private Constructors

        #region Private Methods

        private static Dictionary<string, IXmlValidator> LoadXmlValidators()
        {
            if (xmlsValidators != null)
            {
                return xmlsValidators;
            }

            xmlsValidators = new Dictionary<string, IXmlValidator>();
            var assembly = typeof(ValidatorFactory).Assembly;
            var types = assembly.GetExportedTypes()
                                .Where(w => !w.IsAbstract &&
                                            !w.IsInterface &&
                                            typeof(IXmlValidator).IsAssignableFrom(w))
                                .ToList();

            foreach (var type in types)
            {
                var validator = Activator.CreateInstance(type) as IXmlValidator;
                var key = type.FullName;
                xmlsValidators.Add(key, validator);
            }

            return xmlsValidators;
        }

        #endregion Private Methods

        #region Public Constructors

        static ValidatorFactory()
        {
        }

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Cria uma instância do validador de acordo com o xml definido em <paramref name="xml"/>
        /// </summary>
        /// <param name="xml">XML utilizado para validação</param>
        /// <returns></returns>
        public static IXmlValidator BuidValidator(string xml)
        {
            if (xml is null)
            {
                throw new ArgumentNullException(nameof(xml));
            }

            try
            {
                //localiza o validador do objeto XML
                var validators = LoadXmlValidators();
                var validator = validators.FirstOrDefault(w => w.Value.CanValidate(XDocument.Parse(xml).Root)).Value;

                if (validator.IsNullOrEmpty())
                {
                    return default;
                }

                validator.Xml = xml;

                return validator;
            }
            catch (Exception ex)
            {
                if (ex.GetLastException().GetType() == typeof(XmlException))
                {
                    var exception = (XmlException)ex.GetLastException();

                    Utility.XMLUtility.ImproveInvalidCharacterExceptionInXML(xml, exception);
                }

                ThrowHelper.Instance.Throw(ex);
                throw;
            }
        }

        #endregion Public Methods        
    }
}