using System.Xml.Linq;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Validator.Abstractions;
using Unimake.Business.DFe.Xml.MDFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Validator.MDFe
{
    /// <summary>
    /// <inheritdoc cref="XmlValidatorBase"/>
    /// </summary>
    public class MDFeValidator : XmlValidatorBase
    {
        #region Public Constructors

        /// <summary>
        /// <inheritdoc cref="XmlValidatorBase"/>
        /// </summary>
        public MDFeValidator() =>
            ValidateTag(element => element.NameEquals(nameof(InfMunCarrega.CMunCarrega)) && element.Parent.NameEquals(nameof(InfMunCarrega)) && element.Parent.Parent.NameEquals(nameof(Ide)), Tag =>
            {
                if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
                {
                    throw new ValidatorDFeException("Código do município de carregamento não foi informado. É obrigatório informar o código IBGE do município." +
                        " [TAG: <cMunCarrega> do grupo de tag <ide><infMunCarrega>]");
                }

                if (Tag.Value.Length != 7)
                {
                    throw new ValidatorDFeException("Código do município de carregamento está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                        " [TAG: <cMunCarrega> do grupo de tag <ide><infMunCarrega>]");
                }
            }).ValidateTag(element => element.NameEquals(nameof(EnderEmit.CMun)) && element.Parent.NameEquals(nameof(EnderEmit)), Tag =>
            {
                if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
                {
                    throw new ValidatorDFeException("Código do município do emitente não foi informado. É obrigatório informar o código IBGE do município." +
                        " [TAG: <cMun> do grupo de tag <infMDFe><emit><enderEmit>]");
                }

                if (Tag.Value.Length != 7)
                {
                    throw new ValidatorDFeException("Código do município do emitente está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                        " [TAG: <cMun> do grupo de tag <infMDFe><emit><enderEmit>]");
                }
            }).ValidateTag(element => element.NameEquals(nameof(InfMunDescarga.CMunDescarga)) && element.Parent.NameEquals(nameof(InfMunDescarga)), Tag =>
            {
                if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
                {
                    throw new ValidatorDFeException("Código do município de descarregamento não foi informado. É obrigatório informar o código IBGE do município." +
                        " [TAG: <cMunDescarga> do grupo de tag <infMDFe><infDoc><infMunDescarga>]");
                }

                if (Tag.Value.Length != 7)
                {
                    throw new ValidatorDFeException("Código do município de descarregamento está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                        " [TAG: <cMunDescarga> do grupo de tag <infMDFe><infDoc><infMunDescarga>]");
                }
            });

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// <inheritdoc cref="XmlValidatorBase.CanValidate(XElement)"/>
        /// </summary>
        /// <param name="element"><inheritdoc cref="XmlValidatorBase.CanValidate(XElement)"/></param>
        /// <returns></returns>
        public override bool CanValidate(XElement element) => XMLUtility.DetectDFeType(element.ToString()) == TipoDFe.MDFe;

        #endregion Public Methods
    }
}