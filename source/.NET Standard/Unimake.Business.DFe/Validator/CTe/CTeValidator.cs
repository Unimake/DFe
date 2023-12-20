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
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("O CST do grupo de tributação de ICMS00 está incorreto. Valor informado: " + Tag.Value + " - Valor aceito: 00." +
                        " [TAG: <CST> do grupo de tag <infCTe><Imp><ICMS><ICMS00>]"));
                }

            }).ValidateTag(element => element.NameEquals(nameof(ICMS20.CST)) && element.Parent.NameEquals(nameof(ICMS20)) && element.Parent.Parent.NameEquals(nameof(ICMS)) && element.Parent.Parent.Parent.NameEquals(nameof(Imp)), Tag =>
            {
                if (Tag.Value != "20")
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("O CST do grupo de tributação de ICMS20 está incorreto. Valor informado: " + Tag.Value + " - Valor aceito: 20." +
                        " [TAG: <CST> do grupo de tag <infCTe><Imp><ICMS><ICMS20>]"));
                }
            }).ValidateTag(element => element.NameEquals(nameof(ICMS45.CST)) && element.Parent.NameEquals(nameof(ICMS45)) && element.Parent.Parent.NameEquals(nameof(ICMS)) && element.Parent.Parent.Parent.NameEquals(nameof(Imp)), Tag =>
            {
                if (Tag.Value != "40" && Tag.Value != "41" && Tag.Value != "51")
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("O CST do grupo de tributação de ICMS45 está incorreto. Valor informado: " + Tag.Value + " - Valores aceitos: 40, 41 e 51." +
                        " [TAG: <CST> do grupo de tag <infCTe><Imp><ICMS><ICMS45>]"));
                }
            }).ValidateTag(element => element.NameEquals(nameof(ICMS60.CST)) && element.Parent.NameEquals(nameof(ICMS60)) && element.Parent.Parent.NameEquals(nameof(ICMS)) && element.Parent.Parent.Parent.NameEquals(nameof(Imp)), Tag =>
            {
                if (Tag.Value != "60")
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("O CST do grupo de tributação de ICMS60 está incorreto. Valor informado: " + Tag.Value + " - Valor aceito: 60." +
                        " [TAG: <CST> do grupo de tag <infCTe><Imp><ICMS><ICMS60>]"));
                }
            }).ValidateTag(element => element.NameEquals(nameof(ICMS90.CST)) && element.Parent.NameEquals(nameof(ICMS90)) && element.Parent.Parent.NameEquals(nameof(ICMS)) && element.Parent.Parent.Parent.NameEquals(nameof(Imp)), Tag =>
            {
                if (Tag.Value != "90")
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("O CST do grupo de tributação de ICMS90 está incorreto. Valor informado: " + Tag.Value + " - Valor aceito: 90." +
                        " [TAG: <CST> do grupo de tag <infCTe><Imp><ICMS><ICMS90>]"));
                }
            }).ValidateTag(element => element.NameEquals(nameof(ICMSOutraUF.CST)) && element.Parent.NameEquals(nameof(ICMSOutraUF)) && element.Parent.Parent.NameEquals(nameof(ICMS)) && element.Parent.Parent.Parent.NameEquals(nameof(Imp)), Tag =>
            {
                if (Tag.Value != "90")
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("O CST do grupo de tributação de ICMSOutraUF está incorreto. Valor informado: " + Tag.Value + " - Valor aceito: 90." +
                        " [TAG: <CST> do grupo de tag <infCTe><Imp><ICMS><ICMSOutraUF>]"));
                }
            }).ValidateTag(element => element.NameEquals(nameof(ICMSSN.CST)) && element.Parent.NameEquals(nameof(ICMSSN)) && element.Parent.Parent.NameEquals(nameof(ICMS)) && element.Parent.Parent.Parent.NameEquals(nameof(Imp)), Tag =>
            {
                if (Tag.Value != "90") //Não pode ser 01, apesar do manual estar 01 o schema só aceita 90, acreditamos que o manual está incorreto. Já reclamaram disso no passado, eu até alterei e depois me dei conta que o schema só aceita 90. Wandrey 20/12/2023
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("O CST do grupo de tributação de ICMSSN está incorreto. Valor informado: " + Tag.Value + " - Valor aceito: 90." +
                        " [TAG: <CST> do grupo de tag <infCTe><Imp><ICMS><ICMSSN>]"));
                }
            }).ValidateTag(element => element.NameEquals(nameof(Ide.CMunEnv)) && element.Parent.NameEquals(nameof(Ide)), Tag =>
            {
                if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município de envio do CT-e (de onde o documento foi transmitido) não foi informado. É obrigatório informar o código IBGE do município." +
                        " [TAG: <cMunEnv> do grupo de tag <infCte><ide>]"));
                }

                if (Tag.Value.Length != 7)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município de envio do CT-e (de onde o documento foi transmitido) está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                        " [TAG: <cMunEnv> do grupo de tag <infCte><ide>]"));
                }
            }).ValidateTag(element => element.NameEquals(nameof(Ide.CMunIni)) && element.Parent.NameEquals(nameof(Ide)), Tag =>
            {
                if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município de início da prestação do serviço de transporte não foi informado. É obrigatório informar o código IBGE do município." +
                        " [TAG: <cMunIni> do grupo de tag <infCte><ide>]"));
                }

                if (Tag.Value.Length != 7)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município de início da prestação de serviço de transporte está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                        " [TAG: <cMunIni> do grupo de tag <infCte><ide>]"));
                }
            }).ValidateTag(element => element.NameEquals(nameof(Ide.CMunFim)) && element.Parent.NameEquals(nameof(Ide)), Tag =>
            {
                if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município de término da prestação do serviço de transporte não foi informado. É obrigatório informar o código IBGE do município." +
                        " [TAG: <cMunFim> do grupo de tag <infCte><ide>]"));
                }

                if (Tag.Value.Length != 7)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município de término da prestação de serviço de transporte está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                        " [TAG: <cMunFim> do grupo de tag <infCte><ide>]"));
                }
            }).ValidateTag(element => element.NameEquals(nameof(EnderToma.CMun)) && element.Parent.NameEquals(nameof(EnderToma)), Tag =>
            {
                if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município do tomador do serviço não foi informado. É obrigatório informar o código IBGE do município." +
                        " [TAG: <cMun> do grupo de tag <infCte><toma><enderToma>]"));
                }

                if (Tag.Value.Length != 7)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município do tomador do serviço está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                        " [TAG: <cMun> do grupo de tag <infCte><toma><enderToma>]"));
                }
            }).ValidateTag(element => element.NameEquals(nameof(EnderEmit.CMun)) && element.Parent.NameEquals(nameof(EnderEmit)), Tag =>
            {
                if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município do emitente não foi informado. É obrigatório informar o código IBGE do município." +
                        " [TAG: <cMun> do grupo de tag <infCte><emit><enderEmit>]"));
                }

                if (Tag.Value.Length != 7)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município do emitente está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                        " [TAG: <cMun> do grupo de tag <infCte><emit><enderEmit>]"));
                }
            }).ValidateTag(element => element.NameEquals(nameof(EnderReme.CMun)) && element.Parent.NameEquals(nameof(EnderReme)), Tag =>
            {
                if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município do remetente das mercadorias transportadas não foi informado. É obrigatório informar o código IBGE do município." +
                        " [TAG: <cMun> do grupo de tag <infCte><rem><enderReme>]"));
                }

                if (Tag.Value.Length != 7)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município do remetente das mercadorias transportadas está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                        " [TAG: <cMun> do grupo de tag <infCte><rem><enderReme>]"));
                }
            }).ValidateTag(element => element.NameEquals(nameof(EnderExped.CMun)) && element.Parent.NameEquals(nameof(EnderExped)), Tag =>
            {
                if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município do expedidor da carga não foi informado. É obrigatório informar o código IBGE do município." +
                        " [TAG: <cMun> do grupo de tag <infCte><exped><enderExped>]"));
                }

                if (Tag.Value.Length != 7)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município do expedidor da carga está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                        " [TAG: <cMun> do grupo de tag <infCte><exped><enderExped>]"));
                }
            }).ValidateTag(element => element.NameEquals(nameof(EnderReceb.CMun)) && element.Parent.NameEquals(nameof(EnderReceb)), Tag =>
            {
                if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município do recebedor da carga não foi informado. É obrigatório informar o código IBGE do município." +
                        " [TAG: <cMun> do grupo de tag <infCte><receb><enderReceb>]"));
                }

                if (Tag.Value.Length != 7)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município do recebedor da carga está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                        " [TAG: <cMun> do grupo de tag <infCte><receb><enderReceb>]"));
                }
            }).ValidateTag(element => element.NameEquals(nameof(EnderDest.CMun)) && element.Parent.NameEquals(nameof(EnderDest)), Tag =>
            {
                if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município do destinatário do CTe não foi informado. É obrigatório informar o código IBGE do município." +
                        " [TAG: <cMun> do grupo de tag <infCte><dest><enderDest>]"));
                }

                if (Tag.Value.Length != 7)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município do destinatário do CTe está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                        " [TAG: <cMun> do grupo de tag <infCte><dest><enderDest>]"));
                }
            }).ValidateTag(element => element.NameEquals(nameof(EnderFerro.CMun)) && element.Parent.NameEquals(nameof(EnderFerro)), Tag =>
            {
                if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município das ferrovias envolvidas não foi informado. É obrigatório informar o código IBGE do município." +
                        " [TAG: <cMun> do grupo de tag <infCte><infCTeNorm><infModal><ferrov><trafMut><ferroEnv><enderFerro>]"));
                }

                if (Tag.Value.Length != 7)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município das ferrovias envolvidas está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
                        " [TAG: <cMun> do grupo de tag <infCte><infCTeNorm><infModal><ferrov><trafMut><ferroEnv><enderFerro>]"));
                }
            }).ValidateTag(element => element.NameEquals(nameof(Dest.IE)) && element.Parent.NameEquals(nameof(Dest)) && element.Parent.Parent.NameEquals(nameof(InfCTe)), Tag =>
            {
                //if (!string.IsNullOrWhiteSpace(Tag.Parent.GetValue("CPF")))
                //{
                //    if (!string.IsNullOrWhiteSpace(Tag.Value) && Tag.Value != "ISENTO")
                //    {
                //        ThrowHelper.Instance.Throw(new ValidatorDFeException("O destinatário do CT-e informado é uma pessoa física. Neste caso o conteúdo da TAG de inscrição estadual do destinatário deve ser ISENTO ou em branco." +
                //            " [TAG: <IE> do grupo de tag <infCte><dest>]");
                //    }
                //}
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