using System;
using System.Xml.Linq;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Validator.Abstractions;
using Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Validator.NFe
{
    /// <summary>
    /// <inheritdoc cref="XmlValidatorBase"/>
    /// </summary>
    public class NFeValidator : XmlValidatorBase
    {
        #region Public Constructors

        /// <summary>
        /// <inheritdoc cref="XmlValidatorBase"/>
        /// </summary>
        public NFeValidator() =>
            ValidateTag(element => element.NameEquals(nameof(EnderDest.CMun)), Tag =>
            {
                if(UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
                {
                    throw new Exception("Código do município do destinatário (tag <cMun> da <enderDest>) está sem conteúdo. É obrigatório informar o código IBGE do município.");
                }
            }).ValidateTag(element => element.NameEquals("CEAN"), Tag =>
            {
                var value = Tag.Value;
                var CProd = Tag.Parent.GetValue("cProd");

                if(value.ToUpper().Trim() != "SEM GTIN")
                {
                    if(!string.IsNullOrWhiteSpace(value) && value.Length != 0 && value.Length != 8 && value.Length != 12 && value.Length != 13 && value.Length != 14)
                    {
                        throw new Exception("Código EAN (código de barra) informado (" + value + ") no produto de código " + CProd + " está incorreto. EAN deve ter 0,8,12,13 ou 14 de tamanho e somente números, ou seja, não pode conter letras.");
                    }

                    for(var i = 0; i < value.Length; i++)
                    {
                        if(!"0123456789".Contains(value.Substring(i, 1)))
                        {
                            throw new Exception("Código EAN (código de barra) informado (" + value + ") no produto de código " + CProd + " está incorreto. Não pode conter letras, somente números.");
                        }
                    }
                }
                else
                {
                    if(value != "SEM GTIN")
                    {
                        throw new Exception("Código EAN (código de barra) informado (" + "\"" + value + "\") tem que ser igual a \"SEM GTIN\", ou seja, tudo maiúsculo e sem espaços no final ou inicio da sentença.");
                    }
                }
            });

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// <inheritdoc cref="XmlValidatorBase.CanValidate(XElement)"/>
        /// </summary>
        /// <param name="element"><inheritdoc cref="XmlValidatorBase.CanValidate(XElement)"/></param>
        /// <returns></returns>
        public override bool CanValidate(XElement element) => XMLUtility.DetectDFeType(element.ToString()) == Servicos.TipoDFe.NFe;

        #endregion Public Methods
    }
}