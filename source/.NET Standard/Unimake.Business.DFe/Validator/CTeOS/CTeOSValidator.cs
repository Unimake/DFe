using System;
using System.Xml.Linq;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Validator.Abstractions;
using Unimake.Business.DFe.Xml.CTeOS;

namespace Unimake.Business.DFe.Validator.CTeOS
{
    /// <summary>
    /// <inheritdoc cref="XmlValidatorBase"/>
    /// </summary>
    public class CTeOSValidator : XmlValidatorBase
    {
        #region Private Fields

        private const string permitidos = "01,1B,02,2D,2E,04,06,07,08,8B,09,10,11,13,14,15,16,17,18,20,21,22,23,24,25,26,27,28 e 55";

        #endregion Private Fields

        #region Public Constructors

        /// <summary>
        /// <inheritdoc cref="XmlValidatorBase"/>
        /// </summary>
        public CTeOSValidator() =>
        ValidateTag(element => element.NameEquals(nameof(AutXML.CNPJ)) ||
                               element.NameEquals(nameof(AutXML.CPF)), Tag =>
        {
            var cpf = Tag.Parent.GetValue("CPF");
            var cnpj = Tag.Parent.GetValue("CNPJ");

            if(!string.IsNullOrWhiteSpace(cpf) &&
               !string.IsNullOrWhiteSpace(cnpj))
            {
                throw new Exception("Não é permitido informar conteúdo na TAG <CPF> e <CNPJ>, filhas da TAG <auxXML>, ao mesmo tempo. Somente uma delas pode ter conteúdo.");
            }
        })
        .ValidateTag(element => element.NameEquals(nameof(RefNF.Mod)), Tag =>
        {
            if(!permitidos.Contains(Tag.Value))
            {
                throw new Exception("Conteúdo da TAG <mod>, filha da TAG <infCteSub><tomaICMS><RefNF>, inválido! Valores aceitos: " + permitidos + ".");
            }
        });

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// <inheritdoc cref="XmlValidatorBase.CanValidate(XElement)"/>
        /// </summary>
        /// <param name="element"><inheritdoc cref="XmlValidatorBase.CanValidate(XElement)"/></param>
        /// <returns></returns>
        public override bool CanValidate(XElement element) => XMLUtility.DetectDFeType(element.ToString()) == Servicos.TipoDFe.CTeOS;

        #endregion Public Methods
    }
}