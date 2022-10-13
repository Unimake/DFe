using System.Xml.Linq;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Validator.Abstractions;
using Unimake.Business.DFe.Xml.CTe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Validator.CTe
{
    /// <summary>
    /// <inheritdoc cref="XmlValidatorBase"/>
    /// </summary>
    public class CTeValidator : XmlValidatorBase
    {
        #region Public Constructors

        /// <summary>
        /// <inheritdoc cref="XmlValidatorBase"/>
        /// </summary>
        public CTeValidator() =>
            ValidateTag(element => element.NameEquals(nameof(ICMS00.CST)) && element.Parent.NameEquals(nameof(ICMS00)) && element.Parent.Parent.NameEquals(nameof(ICMS)) && element.Parent.Parent.Parent.NameEquals(nameof(Imp)), Tag =>
            {
                if (Tag.Value != "00")
                {
                    throw new ValidatorDFeException("O CST do grupo de tributação de ICMS00 está incorreto. Valor informado: " + Tag.Value + " - Valor aceito: 00." +
                        " [TAG: <CST> do grupo de tag <infCTe><Imp><ICMS><ICMS00>]");
                }

            }).ValidateTag(element => element.NameEquals(nameof(ICMS20.CST)) && element.Parent.NameEquals(nameof(ICMS20)) && element.Parent.Parent.NameEquals(nameof(ICMS)) && element.Parent.Parent.Parent.NameEquals(nameof(Imp)), Tag =>
            {
                if (Tag.Value != "20")
                {
                    throw new ValidatorDFeException("O CST do grupo de tributação de ICMS20 está incorreto. Valor informado: " + Tag.Value + " - Valor aceito: 20." +
                        " [TAG: <CST> do grupo de tag <infCTe><Imp><ICMS><ICMS20>]");
                }
            }).ValidateTag(element => element.NameEquals(nameof(ICMS45.CST)) && element.Parent.NameEquals(nameof(ICMS45)) && element.Parent.Parent.NameEquals(nameof(ICMS)) && element.Parent.Parent.Parent.NameEquals(nameof(Imp)), Tag =>
            {
                if (Tag.Value != "40" && Tag.Value != "41" && Tag.Value != "51")
                {
                    throw new ValidatorDFeException("O CST do grupo de tributação de ICMS45 está incorreto. Valor informado: " + Tag.Value + " - Valores aceitos: 40, 41 e 51." +
                        " [TAG: <CST> do grupo de tag <infCTe><Imp><ICMS><ICMS45>]");
                }
            }).ValidateTag(element => element.NameEquals(nameof(ICMS60.CST)) && element.Parent.NameEquals(nameof(ICMS60)) && element.Parent.Parent.NameEquals(nameof(ICMS)) && element.Parent.Parent.Parent.NameEquals(nameof(Imp)), Tag =>
            {
                if (Tag.Value != "60")
                {
                    throw new ValidatorDFeException("O CST do grupo de tributação de ICMS60 está incorreto. Valor informado: " + Tag.Value + " - Valor aceito: 60." +
                        " [TAG: <CST> do grupo de tag <infCTe><Imp><ICMS><ICMS60>]");
                }
            }).ValidateTag(element => element.NameEquals(nameof(ICMS90.CST)) && element.Parent.NameEquals(nameof(ICMS90)) && element.Parent.Parent.NameEquals(nameof(ICMS)) && element.Parent.Parent.Parent.NameEquals(nameof(Imp)), Tag =>
            {
                if (Tag.Value != "90")
                {
                    throw new ValidatorDFeException("O CST do grupo de tributação de ICMS90 está incorreto. Valor informado: " + Tag.Value + " - Valor aceito: 90." +
                        " [TAG: <CST> do grupo de tag <infCTe><Imp><ICMS><ICMS90>]");
                }
            }).ValidateTag(element => element.NameEquals(nameof(ICMSOutraUF.CST)) && element.Parent.NameEquals(nameof(ICMSOutraUF)) && element.Parent.Parent.NameEquals(nameof(ICMS)) && element.Parent.Parent.Parent.NameEquals(nameof(Imp)), Tag =>
            {
                if (Tag.Value != "90")
                {
                    throw new ValidatorDFeException("O CST do grupo de tributação de ICMSOutraUF está incorreto. Valor informado: " + Tag.Value + " - Valor aceito: 90." +
                        " [TAG: <CST> do grupo de tag <infCTe><Imp><ICMS><ICMSOutraUF>]");
                }
            }).ValidateTag(element => element.NameEquals(nameof(ICMSSN.CST)) && element.Parent.NameEquals(nameof(ICMSSN)) && element.Parent.Parent.NameEquals(nameof(ICMS)) && element.Parent.Parent.Parent.NameEquals(nameof(Imp)), Tag =>
            {
                if (Tag.Value != "90")
                {
                    throw new ValidatorDFeException("O CST do grupo de tributação de ICMSSN está incorreto. Valor informado: " + Tag.Value + " - Valor aceito: 90." +
                        " [TAG: <CST> do grupo de tag <infCTe><Imp><ICMS><ICMSSN>]");
                }
            });

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// <inheritdoc cref="XmlValidatorBase.CanValidate(XElement)"/>
        /// </summary>
        /// <param name="element"><inheritdoc cref="XmlValidatorBase.CanValidate(XElement)"/></param>
        /// <returns></returns>
        public override bool CanValidate(XElement element) => XMLUtility.DetectDFeType(element.ToString()) == TipoDFe.CTe;

        #endregion Public Methods
    }
}