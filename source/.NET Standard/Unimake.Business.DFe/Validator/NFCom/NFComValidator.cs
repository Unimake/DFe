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
            var cst = Tag.Value;
            var ibscbs = Tag.Parent;

            var ide = ibscbs.Parent.Parent.Parent.GetElement("ide");
            var gCompraGov = ide.GetElement("gCompraGov");

            var det = ibscbs.Parent.Parent;
            var cProd = det.GetValue("cProd");
            var xProd = det.GetValue("xProd");
            var nItem = det.GetAttributeValue("nItem");

            var gIBSCBS = ibscbs.GetElement("gIBSCBS");
            var gIBSUF = gIBSCBS?.GetElement("gIBSUF");
            var gIBSMun = gIBSCBS?.GetElement("gIBSMun");
            var gCBS = gIBSCBS?.GetElement("gCBS");

            switch (cst)
            {
                case "000": // Tributação integral
                    if (gIBSCBS == null)
                    {
                        ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', deve ser preenchido o grupo gIBSCB de IBSCBS." +
                            $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gIBSCBS> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS>]"));
                    }

                    if (gIBSUF != null)
                    {
                        if (gIBSUF.GetElement("gDif") != null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', não é permitido o preenchimento do grupo gDif de gIBSUF." +
                                $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gDif> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS><gIBSCBS><gIBSUF>]"));
                        }

                        if (gIBSUF.GetElement("gRed") != null)
                        {
                            if (gCompraGov == null)
                            {
                                ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}' e o grupo gCompraGov não estiver preenchido em <NFCom><infNFCom><ide>, o preenchimento do grupo gRed dentro de gIBSUF não é permitido." +
                                    $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gRed> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS><gIBSCBS><gIBSUF>]"));
                            }
                        }
                        else
                        {
                            if (gCompraGov != null)
                            {
                                ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}' e o grupo gCompraGov estiver preenchido em <NFCom><infNFCom><ide>, o grupo gRed também deve ser informado dentro de gIBSUF." +
                                    $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gRed> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS><gIBSCBS><gIBSUF>]"));
                            }
                        }
                    }

                    if (gIBSMun != null)
                    {
                        if (gIBSMun.GetElement("gDif") != null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', não é permitido o preenchimento do grupo gDif de gIBSMun." +
                                $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gDif> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS><gIBSCBS><gIBSMun>]"));
                        }

                        if (gIBSMun.GetElement("gRed") != null)
                        {
                            if (gCompraGov == null)
                            {
                                ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}' e o grupo gCompraGov não estiver preenchido em <NFCom><infNFCom><ide>, o preenchimento do grupo gRed dentro de gIBSMun não é permitido." +
                                    $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gRed> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS><gIBSCBS><gIBSMun>]"));
                            }
                        }
                        else
                        {
                            if (gCompraGov != null)
                            {
                                ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}' e o grupo gCompraGov estiver preenchido em <NFCom><infNFCom><ide>, o grupo gRed também deve ser informado dentro de gIBSMun." +
                                    $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gRed> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS><gIBSCBS><gIBSMun>]"));
                            }
                        }
                    }

                    if (gCBS != null)
                    {
                        if (gCBS.GetElement("gDif") != null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', não é permitido o preenchimento do grupo gDif de gCBS." +
                                $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gDif> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS><gIBSCBS><gCBS>]"));
                        }

                        if (gCBS.GetElement("gRed") != null)
                        {
                            if (gCompraGov == null)
                            {
                                ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}' e o grupo gCompraGov não estiver preenchido em <NFCom><infNFCom><ide>, o preenchimento do grupo gRed dentro de gCBS não é permitido." +
                                    $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gRed> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS><gIBSCBS><gCBS>]"));
                            }
                        }
                        else
                        {
                            if (gCompraGov != null)
                            {
                                ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}' e o grupo gCompraGov estiver preenchido em <NFCom><infNFCom><ide>, o grupo gRed também deve ser informado dentro de gCBS." +
                                    $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gRed> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS><gIBSCBS><gCBS>]"));
                            }
                        }
                    }

                    break;

                case "200": // Alíquota reduzida

                    if (gIBSCBS == null)
                    {
                        ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', deve ser preenchido o grupo gIBSCBS de IBSCBS." +
                            $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gIBSCBS> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS>]"));
                    }

                    if (gIBSUF != null)
                    {
                        if (gIBSUF.GetElement("gDif") != null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', não é permitido o preenchimento do grupo gDif de gIBSUF." +
                                $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gDif> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS><gIBSCBS><gIBSUF>]"));
                        }

                        if (gIBSUF.GetElement("gRed") == null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', deve ser preenchido o grupo gRed de gIBSUF." +
                                $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gRed> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS><gIBSCBS><gIBSUF>]"));
                        }
                    }

                    if (gIBSMun != null)
                    {
                        if (gIBSMun.GetElement("gDif") != null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', não é permitido o preenchimento do grupo gDif de gIBSMun." +
                                $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gDif> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS><gIBSCBS><gIBSMun>]"));
                        }

                        if (gIBSMun.GetElement("gRed") == null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', deve ser preenchido o grupo gRed de gIBSMun." +
                                $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gRed> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS><gIBSCBS><gIBSMun>]"));
                        }
                    }

                    if (gCBS != null)
                    {
                        if (gCBS.GetElement("gDif") != null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', não é permitido o preenchimento do grupo gDif de gCBS." +
                                $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gDif> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS><gIBSCBS><gCBS>]"));
                        }

                        if (gCBS.GetElement("gRed") == null)
                        {
                            ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', deve ser preenchido o grupo gRed de gCBS." +
                                $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gRed> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS><gIBSCBS><gCBS>]"));
                        }
                    }

                    break;

                case "410": // Imunidade e não incidência

                    if (gIBSCBS != null)
                    {
                        ThrowHelper.Instance.Throw(new ValidatorDFeException($"Quando o CST de IBSCBS for '{cst}', não é permitido o preenchimento do grupo gIBSCBS de IBSCBS." +
                            $" [Item]: {nItem} [Prod]: {cProd} [xProd]: {xProd} [TAG: <gIBSCBS> do grupo de tag <NFCom><infNFCom><det><imposto><IBSCBS>]"));
                    }

                    break;
            }

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
