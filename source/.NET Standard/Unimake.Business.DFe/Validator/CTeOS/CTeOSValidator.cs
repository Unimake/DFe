using System.Xml.Linq;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Validator.Abstractions;
using Unimake.Business.DFe.Xml.CTeOS;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Validator.CTeOS
{
    /// <summary>
    /// <inheritdoc cref="XmlValidatorBase"/>
    /// </summary>
    public class CTeOSValidator : XmlValidatorBase
    {
        #region Public Constructors

        /// <summary>
        /// <inheritdoc cref="XmlValidatorBase"/>
        /// </summary>
        public CTeOSValidator() =>
        ValidateTag(element => (element.NameEquals(nameof(AutXML.CNPJ)) || element.NameEquals(nameof(AutXML.CPF))) && element.Parent.NameEquals(nameof(AutXML)), Tag =>
        {
            var cpf = Tag.Parent.GetValue("CPF");
            var cnpj = Tag.Parent.GetValue("CNPJ");

            if (!string.IsNullOrWhiteSpace(cpf) &&
               !string.IsNullOrWhiteSpace(cnpj))
            {
                throw new ValidatorDFeException("Não é permitido informar CPF e CNPJ concomitantemente nas informações dos autorizados a fazer download do XML do CTeOS. Somente um pode ser informado." +
                    " [TAG: <CPF> e <CNPJ> do grupo de tag <infCte><autXML>]");
            }
        })
        .ValidateTag(element => element.NameEquals(nameof(RefNF.Mod)) && element.Parent.NameEquals(nameof(RefNF)), Tag =>
        {
            const string permitidos = "01,1B,02,2D,2E,04,06,07,08,8B,09,10,11,13,14,15,16,17,18,20,21,22,23,24,25,26,27,28 e 55";

            if (!permitidos.Contains(Tag.Value))
            {
                throw new ValidatorDFeException("O modelo do documento informado nas informações da NF ou CT emitido pelo tomador é inválido. Valor informado: " + Tag.Value + " - Valores aceitos: " + permitidos +
                    " [TAG: <mod> do grupo de tag <infCte><infCTeNorm><infCteSub><tomaICMS><RefNF>]");
            }
        }).ValidateTag(element => element.NameEquals(nameof(Ide.CMunEnv)) && element.Parent.NameEquals(nameof(Ide)), Tag =>
        {
            if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
            {
                throw new ValidatorDFeException("Código do município de envio do CTeOS (de onde o documento foi transmitido) não foi informado. É obrigatório informar o código IBGE do município." +
                    " [TAG: <cMunEnv> do grupo de tag <infCte><ide>]");
            }

            if (Tag.Value.Length != 7)
            {
                throw new ValidatorDFeException("Código do município de envio do CTeOS (de onde o documento foi transmitido) está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                    " [TAG: <cMunEnv> do grupo de tag <infCte><ide>]");
            }
        }).ValidateTag(element => element.NameEquals(nameof(Ide.CMunIni)) && element.Parent.NameEquals(nameof(Ide)), Tag =>
        {
            if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
            {
                throw new ValidatorDFeException("Código do município de início da prestação do serviço de transporte não foi informado. É obrigatório informar o código IBGE do município." +
                    " [TAG: <cMunIni> do grupo de tag <infCte><ide>]");
            }

            if (Tag.Value.Length != 7)
            {
                throw new ValidatorDFeException("Código do município de início da prestação de serviço de transporte está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                    " [TAG: <cMunIni> do grupo de tag <infCte><ide>]");
            }
        }).ValidateTag(element => element.NameEquals(nameof(Ide.CMunFim)) && element.Parent.NameEquals(nameof(Ide)), Tag =>
        {
            if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
            {
                throw new ValidatorDFeException("Código do município de término da prestação do serviço de transporte não foi informado. É obrigatório informar o código IBGE do município." +
                    " [TAG: <cMunFim> do grupo de tag <infCte><ide>]");
            }

            if (Tag.Value.Length != 7)
            {
                throw new ValidatorDFeException("Código do município de término da prestação de serviço de transporte está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                    " [TAG: <cMunFim> do grupo de tag <infCte><ide>]");
            }
        }).ValidateTag(element => element.NameEquals(nameof(EnderEmit.CMun)) && element.Parent.NameEquals(nameof(EnderEmit)), Tag =>
        {
            if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
            {
                throw new ValidatorDFeException("Código do município do emitente não foi informado. É obrigatório informar o código IBGE do município." +
                    " [TAG: <cMun> do grupo de tag <infCte><emit><enderEmit>]");
            }

            if (Tag.Value.Length != 7)
            {
                throw new ValidatorDFeException("Código do município do emitente está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                    " [TAG: <cMun> do grupo de tag <infCte><emit><enderEmit>]");
            }
        }).ValidateTag(element => element.NameEquals(nameof(EnderToma.CMun)) && element.Parent.NameEquals(nameof(EnderToma)), Tag =>
        {
            if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
            {
                throw new ValidatorDFeException("Código do município do tomador do serviço não foi informado. É obrigatório informar o código IBGE do município." +
                    " [TAG: <cMun> do grupo de tag <infCte><toma><enderToma>]");
            }

            if (Tag.Value.Length != 7)
            {
                throw new ValidatorDFeException("Código do município do tomador do serviço está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                    " [TAG: <cMun> do grupo de tag <infCte><toma><enderToma>]");
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