#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFCe
{
    /// <summary>
    /// Enviar o XML de NFCe para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFCe.Autorizacao")]
    [ComVisible(true)]
#endif
    public class Autorizacao : NFe.Autorizacao
    {
        #region Private Methods

        /// <summary>
        /// Definir as propriedades do QRCode e Link da consulta manual da NFCe
        /// </summary>
        private void MontarQrCode()
        {
            if (ConteudoXML.GetElementsByTagName("enviNFe").Count <= 0)
            {
                throw new Exception("A tag obrigatória <enviNFe> não foi localizada no XML.");
            }
            var elementEnviNFe = (XmlElement)ConteudoXML.GetElementsByTagName("enviNFe")[0];

            if (ConteudoXML.GetElementsByTagName("NFe").Count <= 0)
            {
                throw new Exception("A tag obrigatória <NFe>, do grupo de tag <enviNFe>, não foi localizada no XML.");
            }
            var nodeListNFe = elementEnviNFe.GetElementsByTagName("NFe");

            foreach (XmlNode nodeNFe in nodeListNFe)
            {
                var elementNFe = (XmlElement)nodeNFe;

                if (elementNFe.GetElementsByTagName("infNFeSupl").Count <= 0)
                {
                    if (string.IsNullOrWhiteSpace(Configuracoes.CSC))
                    {
                        throw new Exception("Para montagem do QRCode é necessário informar o conteúdo da propriedade \"Configuracao.CSC\"");
                    }

                    if (Configuracoes.CSCIDToken <= 0)
                    {
                        throw new Exception("Para montagem do QRCode é necessário informar o conteúdo da propriedade \"Configuracao.CSCIDToken\"");
                    }

                    if (elementNFe.GetElementsByTagName("infNFe").Count <= 0)
                    {
                        throw new Exception("A tag obrigatória <infNFe>, do grupo de tag <enviNFe><NFe>, não foi localizada no XML.");
                    }
                    var elementInfNFe = (XmlElement)elementNFe.GetElementsByTagName("infNFe")[0];

                    if (elementInfNFe.GetElementsByTagName("ide").Count <= 0)
                    {
                        throw new Exception("A tag obrigatória <ide>, do grupo de tag <enviNFe><NFe><infNFe>, não foi localizada no XML.");
                    }
                    var elementIde = (XmlElement)elementInfNFe.GetElementsByTagName("ide")[0];

                    if (elementIde.GetElementsByTagName("tpEmis").Count <= 0)
                    {
                        throw new Exception("A tag obrigatória <tpEmis>, do grupo de tag <enviNFe><NFe><infNFe><ide>, não foi localizada no XML.");
                    }
                    if (elementIde.GetElementsByTagName("tpAmb").Count <= 0)
                    {
                        throw new Exception("A tag obrigatória <tpAmb>, do grupo de tag <enviNFe><NFe><infNFe><ide>, não foi localizada no XML.");
                    }
                    if (elementIde.GetElementsByTagName("dhEmi").Count <= 0)
                    {
                        throw new Exception("A tag obrigatória <dhEmi>, do grupo de tag <enviNFe><NFe><infNFe><ide>, não foi localizada no XML.");
                    }
                    if (elementIde.GetElementsByTagName("cUF").Count <= 0)
                    {
                        throw new Exception("A tag obrigatória <cUF>, do grupo de tag <enviNFe><NFe><infNFe><ide>, não foi localizada no XML.");
                    }
                    if (elementIde.GetElementsByTagName("mod").Count <= 0)
                    {
                        throw new Exception("A tag obrigatória <mod>, do grupo de tag <enviNFe><NFe><infNFe><ide>, não foi localizada no XML.");
                    }
                    if (elementIde.GetElementsByTagName("serie").Count <= 0)
                    {
                        throw new Exception("A tag obrigatória <serie>, do grupo de tag <enviNFe><NFe><infNFe><ide>, não foi localizada no XML.");
                    }
                    if (elementIde.GetElementsByTagName("nNF").Count <= 0)
                    {
                        throw new Exception("A tag obrigatória <nNF>, do grupo de tag <enviNFe><NFe><infNFe><ide>, não foi localizada no XML.");
                    }
                    if (elementIde.GetElementsByTagName("cNF").Count <= 0)
                    {
                        throw new Exception("A tag obrigatória <cNF>, do grupo de tag <enviNFe><NFe><infNFe><ide>, não foi localizada no XML.");
                    }

                    var tpEmis = (TipoEmissao)Convert.ToInt32(elementIde.GetElementsByTagName("tpEmis")[0].InnerText);
                    var tpAmb = (TipoAmbiente)Convert.ToInt32(elementIde.GetElementsByTagName("tpAmb")[0].InnerText);
                    var dhEmi = DateTimeOffset.Parse(elementIde.GetElementsByTagName("dhEmi")[0].InnerText);
                    var cUF = elementIde.GetElementsByTagName("cUF")[0].InnerText;
                    var mod = elementIde.GetElementsByTagName("mod")[0].InnerText;
                    var serie = elementIde.GetElementsByTagName("serie")[0].InnerText;
                    var nNF = elementIde.GetElementsByTagName("nNF")[0].InnerText;
                    var cNF = elementIde.GetElementsByTagName("cNF")[0].InnerText;

                    if (elementInfNFe.GetElementsByTagName("emit").Count <= 0)
                    {
                        throw new Exception("A tag obrigatória <emit>, do grupo de tag <enviNFe><NFe><infNFe>, não foi localizada no XML.");
                    }
                    var elementEmit = (XmlElement)elementInfNFe.GetElementsByTagName("emit")[0];

                    var CNPJEmit = string.Empty;
                    var CPFEmit = string.Empty;
                    if (elementEmit.GetElementsByTagName("CNPJ").Count <= 0)
                    {
                        if (elementEmit.GetElementsByTagName("CPF").Count <= 0)
                        {
                            throw new Exception("A tag obrigatória <CNPJ> ou <CPF>, do grupo de tag <enviNFe><NFe><infNFe><emit>, não foi localizada no XML.");
                        }
                        else
                        {
                            CPFEmit = elementEmit.GetElementsByTagName("CPF")[0].InnerText;
                        }
                    }
                    else
                    {
                        CNPJEmit = elementEmit.GetElementsByTagName("CNPJ")[0].InnerText;
                    }

                    var conteudoChaveDFe = new XMLUtility.ConteudoChaveDFe
                    {
                        UFEmissor = (UFBrasil)Convert.ToInt32(cUF),
                        AnoEmissao = dhEmi.ToString("yy"),
                        MesEmissao = dhEmi.ToString("MM"),
                        CNPJCPFEmissor = (string.IsNullOrWhiteSpace(CNPJEmit) ? CPFEmit : CNPJEmit).PadLeft(14, '0'),
                        Modelo = (ModeloDFe)Convert.ToInt32(mod),
                        Serie = Convert.ToInt32(serie),
                        NumeroDoctoFiscal = Convert.ToInt32(nNF),
                        TipoEmissao = (TipoEmissao)(int)tpEmis,
                        CodigoNumerico = cNF
                    };
                    var chave = XMLUtility.MontarChaveNFe(ref conteudoChaveDFe);

                    var urlQrCode = (Configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? Configuracoes.UrlQrCodeHomologacao : Configuracoes.UrlQrCodeProducao);
                    var urlChave = (Configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? Configuracoes.UrlChaveHomologacao : Configuracoes.UrlChaveProducao);
                    string paramLinkQRCode;

                    if (tpEmis == TipoEmissao.ContingenciaOffLine)
                    {
                        if (elementNFe.GetElementsByTagName("total").Count <= 0)
                        {
                            throw new Exception("A tag obrigatória <total>, do grupo de tag <enviNFe><NFe><infNFe>, não foi localizada no XML.");
                        }
                        var elementTotal = (XmlElement)elementInfNFe.GetElementsByTagName("total")[0];

                        if (elementTotal.GetElementsByTagName("ICMSTot").Count <= 0)
                        {
                            throw new Exception("A tag obrigatória <ICMSTot>, do grupo de tag <enviNFe><NFe><infNFe><total>, não foi localizada no XML.");
                        }
                        var elementICMSTot = (XmlElement)elementInfNFe.GetElementsByTagName("ICMSTot")[0];

                        if (elementICMSTot.GetElementsByTagName("vNF").Count <= 0)
                        {
                            throw new Exception("A tag obrigatória <vNF>, do grupo de tag <enviNFe><NFe><infNFe><total><ICMSTot>, não foi localizada no XML.");
                        }
                        var vNF = elementICMSTot.GetElementsByTagName("vNF")[0].InnerText;

                        if (elementNFe.GetElementsByTagName("Signature").Count <= 0)
                        {
                            throw new Exception("A tag obrigatória <Signature>, do grupo de tag <enviNFe><NFe>, não foi localizada no XML.");
                        }
                        var elementSignature = (XmlElement)elementNFe.GetElementsByTagName("Signature")[0];

                        if (elementSignature.GetElementsByTagName("SignedInfo").Count <= 0)
                        {
                            throw new Exception("A tag obrigatória <SignedInfo>, do grupo de tag <enviNFe><NFe><Signature>, não foi localizada no XML.");
                        }
                        var elementSignedInfo = (XmlElement)elementSignature.GetElementsByTagName("SignedInfo")[0];

                        if (elementSignedInfo.GetElementsByTagName("Reference").Count <= 0)
                        {
                            throw new Exception("A tag obrigatória <Reference>, do grupo de tag <enviNFe><NFe><Signature><SignedInfo>, não foi localizada no XML.");
                        }
                        var elementReference = (XmlElement)elementSignedInfo.GetElementsByTagName("Reference")[0];

                        if (elementReference.GetElementsByTagName("DigestValue").Count <= 0)
                        {
                            throw new Exception("A tag obrigatória <Reference>, do grupo de tag <enviNFe><NFe><Signature><SignedInfo><Reference>, não foi localizada no XML.");
                        }
                        var digestValue = elementReference.GetElementsByTagName("DigestValue")[0].InnerText;

                        paramLinkQRCode = chave + "|" +
                            "2" + "|" +
                            ((int)tpAmb).ToString() + "|" +
                            dhEmi.ToString("dd") + "|" +
                            vNF.Trim() + "|" +
                            Converter.ToHexadecimal(digestValue.ToString()) + "|" +
                            Configuracoes.CSCIDToken.ToString();
                    }
                    else
                    {
                        paramLinkQRCode = chave + "|" +
                            "2" + "|" +
                            ((int)tpAmb).ToString() + "|" +
                            Configuracoes.CSCIDToken.ToString();
                    }

                    var hashQRCode = Converter.ToSHA1HashData(paramLinkQRCode.Trim() + Configuracoes.CSC, true);
                    var qrCode = urlQrCode + "?p=" + paramLinkQRCode.Trim() + "|" + hashQRCode.Trim();

                    var namespaceURI = nodeNFe.GetNamespaceOfPrefix("");
                    XmlNode infNFeSuplNode = ConteudoXML.CreateElement("infNFeSupl", namespaceURI);
                    XmlNode qrCodeNode = ConteudoXML.CreateElement("qrCode", namespaceURI);
                    qrCodeNode.InnerText = qrCode;
                    infNFeSuplNode.AppendChild(qrCodeNode);
                    XmlNode urlChaveNode = ConteudoXML.CreateElement("urlChave", namespaceURI);
                    urlChaveNode.InnerText = urlChave;
                    infNFeSuplNode.AppendChild(urlChaveNode);
                    nodeNFe.AppendChild(infNFeSuplNode);
                    var nodeInfNFe = (XmlNode)elementInfNFe;
                    nodeNFe.InsertAfter(infNFeSuplNode, nodeInfNFe);
                }
            }
        }

        #endregion Private Methods

        #region Protected Methods

        /// <summary>
        /// Efetuar um ajuste no XML da NFCe logo depois de assinado
        /// </summary>
        protected override void AjustarXMLAposAssinado()
        {
            MontarQrCode();
            base.AjustarXMLAposAssinado();
        }

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            var validar = new ValidarSchema();
            validar.Validar(ConteudoXML, TipoDFe.NFe.ToString() + "." + Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

            if (!validar.Success)
            {
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }

        #endregion Protected Methods

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="enviNFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public Autorizacao(EnviNFe enviNFe, Configuracao configuracao) : base(enviNFe, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public Autorizacao(string conteudoXML, Configuracao configuracao) : base(conteudoXML, configuracao) { }

        /// <summary>
        /// Construtor
        /// </summary>
        public Autorizacao() : base() { }

        #endregion Public Constructors
    }
}