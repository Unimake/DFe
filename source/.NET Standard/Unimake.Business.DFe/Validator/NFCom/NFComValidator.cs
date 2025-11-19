using System.Xml.Linq;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Validator.Abstractions;
using Unimake.Business.DFe.Xml.NFCom;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Validator.NFCom
{
    /// <summary>
    /// <inheritdoc cref="XmlValidatorBase"/>
    /// </summary>
    public class NFComValidator : XmlValidatorBase
    {
        #region Public Contructors

        /// <summary>
        /// <inheritdoc cref="XmlValidatorBase"/>
        /// </summary>
        public NFComValidator() => ValidateTag(element => element.NameEquals(nameof(IBSCBS.CST)) && element.Parent.NameEquals(nameof(IBSCBS)), Tag =>
        {
            

        });

        #endregion Public Contructors

        #region Public Methods

        /// <summary>
        /// <inheritdoc cref="XmlValidatorBase.CanValidate(XElement)"/>
        /// </summary>
        /// <param name="element"><inheritdoc cref="XmlValidatorBase.CanValidate(XElement)"/></param>
        /// <returns></returns>
        public override bool CanValidate(XElement element) => XMLUtility.DetectDFeType(element.ToString()) == TipoDFe.NFCom;

        #endregion Public Methods
    }
}
