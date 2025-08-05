using System.Xml.Linq;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Validator.Abstractions;
using Unimake.Business.DFe.Xml.CTeOS;
using XmlCTe = Unimake.Business.DFe.Xml.CTe;
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
                ThrowHelper.Instance.Throw(new ValidatorDFeException("Não é permitido informar CPF e CNPJ concomitantemente nas informações dos autorizados a fazer download do XML do CTeOS. Somente um pode ser informado." +
                    " [TAG: <CPF> e <CNPJ> do grupo de tag <infCte><autXML>]"));
            }
        })
        .ValidateTag(element => element.NameEquals(nameof(RefNF.Mod)) && element.Parent.NameEquals(nameof(RefNF)), Tag =>
        {
            const string permitidos = "01,1B,02,2D,2E,04,06,07,08,8B,09,10,11,13,14,15,16,17,18,20,21,22,23,24,25,26,27,28 e 55";

            if (!permitidos.Contains(Tag.Value))
            {
                ThrowHelper.Instance.Throw(new ValidatorDFeException("O modelo do documento informado nas informações da NF ou CT emitido pelo tomador é inválido. Valor informado: " + Tag.Value + " - Valores aceitos: " + permitidos +
                    " [TAG: <mod> do grupo de tag <infCte><infCTeNorm><infCteSub><tomaICMS><RefNF>]"));
            }
        }).ValidateTag(element => element.NameEquals(nameof(Ide.CMunEnv)) && element.Parent.NameEquals(nameof(Ide)), Tag =>
        {
            if (UConvert.ToInt(Tag.Value) <= 0 || Tag.Value == null)
            {
                ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município de envio do CTeOS (de onde o documento foi transmitido) não foi informado. É obrigatório informar o código IBGE do município." +
                    " [TAG: <cMunEnv> do grupo de tag <infCte><ide>]"));
            }

            if (Tag.Value.Length != 7)
            {
                ThrowHelper.Instance.Throw(new ValidatorDFeException("Código do município de envio do CTeOS (de onde o documento foi transmitido) está incorreto. Código informado deve ter 7 dígitos. Valor informado: " + Tag.Value +
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
        }).ValidateTag(element => element.NameEquals(nameof(Ide)), Tag =>
        {
            var tpCte = Tag.GetValue("tpCTe"); // Tipo do CTe
            var tpServ = Tag.GetValue("tpServ"); //Transporte de pessoas
            var modal = Tag.GetValue("modal"); //Rodoviário

            //0=Normal 3=Substituição | 6=Transporte de pessoas | 01=Modal Rodoviário
            if ((tpCte == "0" || tpCte == "3") && tpServ == "6" && modal == "01")
            {
                var UFIni = Tag.GetValue("UFIni"); //UF inicial do transporte
                var UFFim = Tag.GetValue("UFFim"); //UF Final do transporte

                if (Tag.Parent.GetElement("infCTeNorm") == null)
                {
                    ThrowHelper.Instance.Throw(new ValidatorDFeException("Para operações com CTe normal/substituição (<tpCTe> = 1 ou 3) de transporte rodoviário de pessoas é obrigatório informar o grupo de tag <infCTeNorm>." +
                        " [TAG: <infCTeNorm> do grupo de tag <infCte>]"));
                }

                if (UFIni != "EX" && UFFim != "EX")
                {
                    if (UFIni != UFFim) //Operação Interestadual
                    {

                        if (string.IsNullOrWhiteSpace(Tag.Parent.GetElement("infCTeNorm").GetElement("infModal").GetElement("rodoOS").GetValue("TAF")))
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException("Para operações interestaduais com CTe normal/substituição de transporte rodoviário de pessoas é obrigatório informar o conteúdo da tag <TAF> (Termo de Autorização de Fretamento) no CTe." +
                                " [TAG: <TAF> do grupo de tag <infCte><infCTeNorm><infModal><rodoOS>]"));
                        }

                        if (!string.IsNullOrWhiteSpace(Tag.Parent.GetElement("infCTeNorm").GetElement("infModal").GetElement("rodoOS").GetValue("NroRegEstadual")))
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException("Para operações interestaduais com CTe normal/substituição de transporte rodoviário de pessoas não pode informar a tag <NroRegEstadual> (Número do Registro Estadual) no CTe." +
                                " [TAG: <NroRegEstadual> do grupo de tag <infCte><infCTeNorm><infModal><rodoOS>]"));
                        }
                    }
                    else //Operação Estadual
                    {
                        if (string.IsNullOrWhiteSpace(Tag.Parent.GetElement("infCTeNorm").GetElement("infModal").GetElement("rodoOS").GetValue("NroRegEstadual")))
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException("Para operações estaduais com CTe normal/substituição de transporte rodoviário de pessoas é obrigatório informar o conteúdo da tag <NroRegEstadual> (Número do Registro Estadual) no CTe." +
                                " [TAG: <NroRegEstadual> do grupo de tag <infCte><infCTeNorm><infModal><rodoOS>]"));
                        }

                        if (!string.IsNullOrWhiteSpace(Tag.Parent.GetElement("infCTeNorm").GetElement("infModal").GetElement("rodoOS").GetValue("TAF")))
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException("Para operações estaduais com CTe normal/substituição de transporte rodoviário de pessoas não pode informar a tag <TAF> (Termo de Autorização de Fretamento) no CTe." +
                                " [TAG: <TAF> do grupo de tag <infCte><infCTeNorm><infModal><rodoOS>]"));
                        }
                    }
                }
            }
        }).ValidateTag(element => element.NameEquals(nameof(XmlCTe.IBSCBS.CST)) && element.Parent.NameEquals(nameof(XmlCTe.IBSCBS)), Tag =>
        {
            var cst = Tag.Value;
            var ibscbs = Tag.Parent;

            var emit = ibscbs.Parent.Parent.GetElement("emit");
            var CRTEmitente = emit.GetValue("CRT");

            var ide = ibscbs.Parent.Parent.GetElement("ide");
            var gCompraGov = ide.GetElement("gCompraGov");

            var gIBSCBS = ibscbs.GetElement("gIBSCBS");

            var gIBSUF = gIBSCBS?.GetElement("gIBSUF");
            var gIBSMun = gIBSCBS?.GetElement("gIBSMun");
            var gCBS = gIBSCBS?.GetElement("gCBS");

            switch (cst)
            {
                case "000": // Tributação integral
                    if (gIBSCBS == null)
                    {
                        // Verifica se é regime normal para ficar de acordo com a rejeição 310
                        if (CRTEmitente == ((int)CRT.RegimeNormal).ToString())
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}' e o CRT do emitente for igual a {(int)CRT.RegimeNormal} - {CRT.RegimeNormal}, deve ser preenchido o grupo gIBSCBS de IBSCBS. " +
                            $"[TAG: <gIBSCBS> do grupo de tag <CTe><infCte><det><imp><IBSCBS>]"));
                        }

                    }

                    if (gIBSUF != null)
                    {
                        if (gIBSUF.GetElement("gDif") != null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', não deve ser preenchido o grupo gDif de gIBSUF. " +
                                $"[TAG: <gDif> do grupo de tag <CTe><infCte><det><imp><IBSCBS><gIBSCBS><gIBSUF>]"));
                        }

                        if (gIBSUF.GetElement("gRed") != null)
                        {
                            if (gCompraGov == null)
                            {
                                ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}' e o grupo gCompraGov não estiver preenchido em <CTe><infCTe><ide>, " +
                                    $"o preenchimento do grupo gRed dentro de gIBSUF não é permitido. " +
                                $"[TAG: <gRed> do grupo de tag <CTe><infCte><det><imp><IBSCBS><gIBSCBS><gIBSUF>]"));
                            }
                        }
                        else
                        {
                            if (gCompraGov != null)
                            {
                                ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}' e o grupo gCompraGov estiver preenchido em <CTe><infCTe><ide>, " +
                                    $"o grupo gRed deve ser informado dentro de gIBSUF. " +
                                $"[TAG: <gRed> do grupo de tag <CTe><infCte><det><imp><IBSCBS><gIBSCBS><gIBSUF>]"));
                            }
                        }
                    }

                    if (gIBSMun != null)
                    {
                        if (gIBSMun.GetElement("gDif") != null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', não deve ser preenchido o grupo gDif de gIBSMun. " +
                            $"[TAG: <gDif> do grupo de tag <CTe><infCte><det><imp><IBSCBS><gIBSCBS><gIBSMun>]"));
                        }

                        if (gIBSMun.GetElement("gRed") != null)
                        {
                            if (gCompraGov == null)
                            {
                                ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}' e o grupo gCompraGov não estiver preenchido em <CTe><infCTe><ide>, " +
                                    $"o preenchimento do grupo gRed dentro de gIBSMun não é permitido. " +
                                $"[TAG: <gRed> do grupo de tag <CTe><infCte><det><imp><IBSCBS><gIBSCBS><gIBSMun>]"));
                            }
                        }
                        else
                        {
                            if (gCompraGov != null)
                            {
                                ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}' e o grupo gCompraGov estiver preenchido em <CTe><infCTe><ide>, " +
                                    $"o grupo gRed deve ser informado dentro de gIBSMun. " +
                                $"[TAG: <gRed> do grupo de tag <CTe><infCte><det><imp><IBSCBS><gIBSCBS><gIBSMun>]"));
                            }
                        }
                    }

                    if (gCBS != null)
                    {
                        if (gCBS.GetElement("gDif") != null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', não deve ser preenchido o grupo gDif de gCBS. " +
                            $"[TAG: <gDif> do grupo de tag <CTe><infCte><det><imp><IBSCBS><gIBSCBS><gCBS>]"));
                        }

                        if (gCBS.GetElement("gRed") != null)
                        {
                            if (gCompraGov == null)
                            {
                                ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}' e o grupo gCompraGov não estiver preenchido em <CTe><infCTe><ide>, " +
                                    $"o preenchimento do grupo gRed dentro de gCBS não é permitido. " +
                                $"[TAG: <gRed> do grupo de tag <CTe><infCte><det><imp><IBSCBS><gIBSCBS><gCBS>]"));
                            }
                        }
                        else
                        {
                            if (gCompraGov != null)
                            {
                                ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}' e o grupo gCompraGov estiver preenchido em <CTe><infCTe><ide>, " +
                                    $"o grupo gRed deve ser informado dentro de gCBS. " +
                                $"[TAG: <gRed> do grupo de tag <CTe><infCte><det><imp><IBSCBS><gIBSCBS><gCBS>]"));
                            }
                        }
                    }

                    break;

                case "200": // Alíquota reduzida

                    if (gIBSCBS == null)
                    {
                        // Verifica se é regime normal para ficar de acordo com a rejeição 310
                        if (CRTEmitente == ((int)CRT.RegimeNormal).ToString())
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}' e o CRT do emitente for igual a {(int)CRT.RegimeNormal} - {CRT.RegimeNormal}, deve ser preenchido o grupo gIBSCBS de IBSCBS. " +
                            $"[TAG: <gIBSCBS> do grupo de tag <CTe><infCte><det><imp><IBSCBS>]"));
                        }
                    }

                    if (gIBSUF != null)
                    {
                        if (gIBSUF.GetElement("gDif") != null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', não deve ser preenchido o grupo gDif de gIBSUF. " +
                        $"[TAG: <gDif> do grupo de tag <CTe><infCte><det><imp><IBSCBS><gIBSCBS><gIBSUF>]"));
                        }

                        if (gIBSUF.GetElement("gRed") == null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', deve ser preenchido o grupo gRed de gIBSUF. " +
                        $"[TAG: <gRed> do grupo de tag <CTe><infCte><det><imp><IBSCBS><gIBSCBS><gIBSUF>]"));
                        }
                    }

                    if (gIBSMun != null)
                    {
                        if (gIBSMun.GetElement("gDif") != null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', não deve ser preenchido o grupo gDif de gIBSMun. " +
                        $"[TAG: <gDif> do grupo de tag <CTe><infCte><det><imp><IBSCBS><gIBSCBS><gIBSMun>]"));
                        }

                        if (gIBSMun.GetElement("gRed") == null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', deve ser preenchido o grupo gRed de gIBSMun. " +
                        $"[TAG: <gRed> do grupo de tag <CTe><infCte><det><imp><IBSCBS><gIBSCBS><gIBSMun>]"));
                        }
                    }

                    if (gCBS != null)
                    {
                        if (gCBS.GetElement("gDif") != null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', não deve ser preenchido o grupo gDif de gCBS. " +
                        $"[TAG: <gDif> do grupo de tag <CTe><infCte><det><imp><IBSCBS><gIBSCBS><gCBS>]"));
                        }

                        if (gCBS.GetElement("gRed") == null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', deve ser preenchido o grupo gRed de gCBS. " +
                        $"[TAG: <gCBS> do grupo de tag <CTe><infCte><det><imp><IBSCBS><gIBSCBS><gCBS>]"));
                        }
                    }

                    break;

                case "410": // Imunidade e não incidência

                    if (gIBSCBS != null)
                    {
                        ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', não deve ser preenchido o grupo gIBSCBS de IBSCBS. " +
                            $"[TAG: <gIBSCBS> do grupo de tag <CTe><infCte><det><imp><IBSCBS>]"));
                    }

                    break;
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