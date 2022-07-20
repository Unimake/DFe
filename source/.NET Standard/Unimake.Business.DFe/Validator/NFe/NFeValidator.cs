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
                if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
                {
                    throw new Exception("Código do município do destinatário da nota está sem conteúdo. É obrigatório informar o código IBGE do município. [TAG: <cMun> do grupo de tag <dest><enderDest>]");
                }
            }).ValidateTag(element => element.NameEquals("CEAN"), Tag =>
            {
                var value = Tag.Value;
                var cProd = Tag.Parent.GetValue("cProd");
                var xProd = Tag.Parent.GetValue("xProd");

                if (value.ToUpper().Trim() != "SEM GTIN")
                {
                    if (!string.IsNullOrWhiteSpace(value) && value.Length != 0 && value.Length != 8 && value.Length != 12 && value.Length != 13 && value.Length != 14)
                    {
                        throw new Exception("Código EAN (código de barra) \"" + value + "\" informado no produto está incorreto. EAN deve ter 0, 8, 12, 13 ou 14 de tamanho e somente números, ou seja, não pode conter letras." +
                            " [Item: " + cProd + " - " + xProd + "] [TAG: <cEAN> do grupo de tag <det><prod>]");
                    }

                    for (var i = 0; i < value.Length; i++)
                    {
                        if (!"0123456789".Contains(value.Substring(i, 1)))
                        {
                            throw new Exception("Código EAN (código de barra) \"" + value + "\" informado no produto está incorreto. Não pode conter letras, somente números." +
                                " [Item: " + cProd + " - " + xProd + "] [TAG: <cEAN> do grupo de tag <det><prod>]");
                        }
                    }
                }
                else
                {
                    if (value != "SEM GTIN")
                    {
                        throw new Exception("Código EAN (código de barra) \"" + value + "\" informado no produto tem que ser igual a \"SEM GTIN\", ou seja, tudo maiúsculo e sem espaços no final ou inicio da sentença." +
                            " [Item: " + cProd + " - " + xProd + "] [TAG: <cEAN> do grupo de tag <det><prod>]");
                    }
                }
            }).ValidateTag(element => element.NameEquals("CEANTrib"), Tag =>
            {
                var value = Tag.Value;
                var cProd = Tag.Parent.GetValue("cProd");
                var xProd = Tag.Parent.GetValue("xProd");

                if (value.ToUpper().Trim() != "SEM GTIN")
                {
                    if (!string.IsNullOrWhiteSpace(value) && value.Length != 0 && value.Length != 8 && value.Length != 12 && value.Length != 13 && value.Length != 14)
                    {
                        throw new Exception("Código EAN Tributável (código de barra) \"" + value + "\" informado no produto está incorreto. EAN deve ter 0, 8, 12, 13 ou 14 de tamanho e somente números, ou seja, não pode conter letras." +
                            " [Item: " + cProd + " - " + xProd + "] [TAG: <cEANTrib> do grupo de tag <det><prod>]");
                    }

                    for (var i = 0; i < value.Length; i++)
                    {
                        if (!"0123456789".Contains(value.Substring(i, 1)))
                        {
                            throw new Exception("Código EAN Tributável (código de barra) \"" + value + "\" informado no produto está incorreto. Não pode conter letras, somente números." +
                                " [Item: " + cProd + " - " + xProd + "] [TAG: <cEANTrib> do grupo de tag <det><prod>]");
                        }
                    }
                }
                else
                {
                    if (value != "SEM GTIN")
                    {
                        throw new Exception("Código EAN Tributável (código de barra) \"" + value + "\" informado no produto tem que ser igual a \"SEM GTIN\", ou seja, tudo maiúsculo e sem espaços no final ou inicio da sentença." +
                            " [Item: " + cProd + " - " + xProd + "] [TAG: <cEANTrib> do grupo de tag <det><prod>]");
                    }
                }
            }).ValidateTag(element => element.NameEquals(nameof(RefNF.Mod)) && element.Parent.NameEquals(nameof(RefNF)), Tag =>
            {
                var value = Tag.Value;

                if (value != "01" && value != "02")
                {
                    throw new Exception("O modelo do documento fiscal informado nos documentos referenciados está incorreto. Valor informado foi " + value + ", mas, só é permitido informar 01 ou 02. [TAG: <mod> do grupo de tag <refNF><NFref>]");
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