using System;
using System.Xml.Linq;
using Unimake.Business.DFe.Servicos;
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
            ValidateTag(element => element.NameEquals(nameof(EnderDest.CMun)) && element.Parent.NameEquals(nameof(EnderDest)), Tag =>
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
                var nItem = Tag.Parent.Parent.Parent.GetAttributeValue("nItem");

                if (value.ToUpper().Trim() != "SEM GTIN")
                {
                    if (!string.IsNullOrWhiteSpace(value) && value.Length != 0 && value.Length != 8 && value.Length != 12 && value.Length != 13 && value.Length != 14)
                    {
                        throw new Exception("Código EAN (código de barra) \"" + value + "\" informado no produto está incorreto. EAN deve ter 0, 8, 12, 13 ou 14 de tamanho e somente números, ou seja, não pode conter letras." +
                            " [Item: " + nItem + "] [cProd: " + cProd + "] [xProd: " + xProd + "] [TAG: <cEAN> do grupo de tag <det><prod>]");
                    }

                    for (var i = 0; i < value.Length; i++)
                    {
                        if (!"0123456789".Contains(value.Substring(i, 1)))
                        {
                            throw new Exception("Código EAN (código de barra) \"" + value + "\" informado no produto está incorreto. Não pode conter letras, somente números." +
                                " [Item: " + nItem + "] [cProd: " + cProd + "] [xProd: " + xProd + "] [TAG: <cEAN> do grupo de tag <det><prod>]");
                        }
                    }
                }
                else
                {
                    if (value != "SEM GTIN")
                    {
                        throw new Exception("Código EAN (código de barra) \"" + value + "\" informado no produto tem que ser igual a \"SEM GTIN\", ou seja, tudo maiúsculo e sem espaços no final ou inicio da sentença." +
                            " [Item: " + nItem + "] [cProd: " + cProd + "] [xProd: " + xProd + "] [TAG: <cEAN> do grupo de tag <det><prod>]");
                    }
                }
            }).ValidateTag(element => element.NameEquals("CEANTrib"), Tag =>
            {
                var value = Tag.Value;
                var cProd = Tag.Parent.GetValue("cProd");
                var xProd = Tag.Parent.GetValue("xProd");
                var nItem = Tag.Parent.Parent.Parent.GetAttributeValue("nItem");

                if (value.ToUpper().Trim() != "SEM GTIN")
                {
                    if (!string.IsNullOrWhiteSpace(value) && value.Length != 0 && value.Length != 8 && value.Length != 12 && value.Length != 13 && value.Length != 14)
                    {
                        throw new Exception("Código EAN Tributável (código de barra) \"" + value + "\" informado no produto está incorreto. EAN deve ter 0, 8, 12, 13 ou 14 de tamanho e somente números, ou seja, não pode conter letras." +
                            " [Item: " + nItem + "] [cProd: " + cProd + "] [xProd: " + xProd + "] [TAG: <cEANTrib> do grupo de tag <det><prod>]");
                    }

                    for (var i = 0; i < value.Length; i++)
                    {
                        if (!"0123456789".Contains(value.Substring(i, 1)))
                        {
                            throw new Exception("Código EAN Tributável (código de barra) \"" + value + "\" informado no produto está incorreto. Não pode conter letras, somente números." +
                                " [Item: " + nItem + "] [cProd: " + cProd + "] [xProd: " + xProd + "] [TAG: <cEANTrib> do grupo de tag <det><prod>]");
                        }
                    }
                }
                else
                {
                    if (value != "SEM GTIN")
                    {
                        throw new Exception("Código EAN Tributável (código de barra) \"" + value + "\" informado no produto tem que ser igual a \"SEM GTIN\", ou seja, tudo maiúsculo e sem espaços no final ou inicio da sentença." +
                            " [Item: " + nItem + "] [cProd: " + cProd + "] [xProd: " + xProd + "] [TAG: <cEANTrib> do grupo de tag <det><prod>]");
                    }
                }
            }).ValidateTag(element => element.NameEquals(nameof(RefNF.Mod)) && element.Parent.NameEquals(nameof(RefNF)), Tag =>
            {
                var value = Tag.Value;

                if (value != "01" && value != "02")
                {
                    throw new Exception("O modelo do documento fiscal informado nos documentos referenciados está incorreto. Valor informado foi " + value + ", mas, só é permitido informar 01 ou 02. [TAG: <mod> do grupo de tag <NFref><refNF>]");
                }
            }).ValidateTag(element => element.NameEquals(nameof(ISSQN.CListServ)) && element.Parent.NameEquals(nameof(ISSQN)), Tag =>
            {
                var value = Tag.Value;
                var cProd = Tag.Parent.Parent.Parent.GetValue("cProd");
                var xProd = Tag.Parent.Parent.Parent.GetValue("xProd");
                var nItem = Tag.Parent.Parent.Parent.GetAttributeValue("nItem");

                if (string.IsNullOrWhiteSpace(value))
                {
                    throw new Exception("Código do item da lista de serviços não foi informado. Em caso de serviços é obrigatório informar este conteúdo." +
                        " [Item: " + nItem + "] [cProd: " + cProd + "] [xProd: " + xProd + "] [TAG: <cListServ> do grupo de tag <det><imposto><ISSQN>]");
                }
                else if (value.Length != 5 || value.Substring(2, 1) != ".")
                {
                    throw new Exception("Código do item da lista de serviços informado está com o formato incorreto. O formato deve ser 99.99, ou seja, 2 dígitos seguidos de um ponto e mais 2 dígitos. Valor informado: " + value + "." +
                        " [Item: " + nItem + "] [cProd: " + cProd + "] [xProd: " + xProd + "] [TAG: <cListServ> do grupo de tag <det><imposto><ISSQN>]");
                }
                else
                {
                    try
                    {
                        var listaServicoISSQN = (ListaServicoISSQN)Enum.Parse(typeof(ListaServicoISSQN), "Servico" + value.Replace(".", ""));
                    }
                    catch
                    {
                        throw new Exception("Código do item da lista de serviços informado não existe na tabela da ABRASF. Valor informado: " + value + "." +
                            " [Item: " + nItem + "] [cProd: " + cProd + "] [xProd: " + xProd + "] [TAG: <cListServ> do grupo de tag <det><imposto><ISSQN>]");
                    }
                }
            }).ValidateTag(element => element.NameEquals(nameof(ISSQN.IndISS)) && element.Parent.NameEquals(nameof(ISSQN)), Tag =>
            {
                var value = Tag.Value;
                var cProd = Tag.Parent.Parent.Parent.GetValue("cProd");
                var xProd = Tag.Parent.Parent.Parent.GetValue("xProd");
                var nItem = Tag.Parent.Parent.Parent.GetAttributeValue("nItem");

                if (string.IsNullOrWhiteSpace(value))
                {
                    throw new Exception("É obrigatório informar o indicador da exigibilidade do ISS para itens de prestação de serviços e o mesmo não foi informado." +
                        " [Item: " + nItem + "] [cProd: " + cProd + "] [xProd: " + xProd + "] [TAG: <indISS> do grupo de tag <det><imposto><ISSQN>]");
                }
                else
                {
                    try
                    {
                        var indISS = (IndicadorExigibilidadeISSQN)Enum.Parse(typeof(IndicadorExigibilidadeISSQN), Enum.GetName(typeof(IndicadorExigibilidadeISSQN), Convert.ToInt32(value)));
                    }
                    catch
                    {
                        throw new Exception("O indicador de exigibilidade do ISS informado no item de prestação de serviço está incorreto. Valor informado: " + value + " - Valores aceitos: 1, 2, 3, 4, 5, 6 e 7." +
                            " [Item: " + nItem + "] [cProd: " + cProd + "] [xProd: " + xProd + "] [TAG: <indISS> do grupo de tag <det><imposto><ISSQN>]");
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