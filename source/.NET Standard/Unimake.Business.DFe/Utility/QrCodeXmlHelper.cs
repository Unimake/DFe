using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Fornece rotinas centralizadas para montagem e inclusão de grupos suplementares de QRCode
    /// nos XMLs de documentos fiscais eletrônicos.
    /// </summary>
    /// <remarks>
    /// Esta classe foi criada para eliminar duplicidade de código nos serviços de autorização,
    /// mantendo em um único ponto as regras de:
    /// <list type="bullet">
    /// <item><description>Validação estrutural mínima do XML necessário para o QRCode;</description></item>
    /// <item><description>Montagem da chave de acesso de cada tipo de documento;</description></item>
    /// <item><description>Composição da URL/parâmetros do QRCode conforme ambiente e emissão;</description></item>
    /// <item><description>Inclusão do grupo suplementar (<c>inf*Supl</c>) no ponto correto do XML.</description></item>
    /// </list>
    /// </remarks>
    internal static class QrCodeXmlHelper
    {
        /// <summary>
        /// Monta e inclui o grupo suplementar <c>infNFeSupl</c> com as tags de QRCode para NFC-e.
        /// </summary>
        /// <param name="conteudoXml">Documento XML da NFC-e já carregado em memória.</param>
        /// <param name="configuracoes">Configurações do serviço (ambiente, URLs, CSC, token e certificado).</param>
        /// <remarks>
        /// O método processa todas as tags <c>NFe</c> dentro do lote <c>enviNFe</c>.
        /// Caso o grupo suplementar já exista em determinada nota, ele não é recriado.
        /// </remarks>
        /// <exception cref="Exception">
        /// Lançada quando alguma tag obrigatória para cálculo da chave/QRCode não é localizada
        /// ou quando os parâmetros de configuração obrigatórios para o layout do QRCode não são informados.
        /// </exception>
        public static void MontarQrCodeNFCe(XmlDocument conteudoXml, Configuracao configuracoes)
        {
            if (conteudoXml.GetElementsByTagName("enviNFe").Count <= 0)
            {
                throw new Exception("A tag obrigatória <enviNFe> não foi localizada no XML.");
            }
            var elementEnviNFe = (XmlElement)conteudoXml.GetElementsByTagName("enviNFe")[0];

            if (conteudoXml.GetElementsByTagName("NFe").Count <= 0)
            {
                throw new Exception("A tag obrigatória <NFe>, do grupo de tag <enviNFe>, não foi localizada no XML.");
            }
            var nodeListNFe = elementEnviNFe.GetElementsByTagName("NFe");

            foreach (XmlNode nodeNFe in nodeListNFe)
            {
                var elementNFe = (XmlElement)nodeNFe;

                if (elementNFe.GetElementsByTagName("infNFeSupl").Count <= 0)
                {
                    if (configuracoes.VersaoQRCodeNFCe <= 2)
                    {
                        if (string.IsNullOrWhiteSpace(configuracoes.CSC))
                        {
                            throw new Exception("Para montagem do QRCode é necessário informar o conteúdo da propriedade \"Configuracao.CSC\"");
                        }

                        if (configuracoes.CSCIDToken <= 0)
                        {
                            throw new Exception("Para montagem do QRCode é necessário informar o conteúdo da propriedade \"Configuracao.CSCIDToken\"");
                        }
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

                    var tpDest = string.Empty;
                    var ifDest = string.Empty;
                    if (elementInfNFe.GetElementsByTagName("dest").Count > 0)
                    {
                        var elementDest = (XmlElement)elementInfNFe.GetElementsByTagName("dest")[0];

                        if (elementDest.GetElementsByTagName("CNPJ").Count > 0)
                        {
                            tpDest = "1";
                            ifDest = elementDest.GetElementsByTagName("CNPJ")[0].InnerText;
                        }
                        else if (elementDest.GetElementsByTagName("CPF").Count > 0)
                        {
                            tpDest = "2";
                            ifDest = elementDest.GetElementsByTagName("CPF")[0].InnerText;
                        }
                        else if (elementDest.GetElementsByTagName("idEstrangeiro").Count > 0)
                        {
                            tpDest = "3";
                        }
                        else
                        {
                            throw new Exception("A tag obrigatória <CNPJ>, <CPF> ou <idEstrangeiro>, do grupo de tag <enviNFe><NFe><infNFe><dest>, não foi localizada no XML.");
                        }
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

                    var urlQrCode = configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? configuracoes.UrlQrCodeHomologacao : configuracoes.UrlQrCodeProducao;
                    var urlChave = configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? configuracoes.UrlChaveHomologacao : configuracoes.UrlChaveProducao;
                    var paramLinkQRCode = chave + "|" + configuracoes.VersaoQRCodeNFCe.ToString() + "|" + ((int)tpAmb).ToString();

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

                        paramLinkQRCode += "|" + dhEmi.ToString("dd") + "|" + vNF.Trim();

                        if (configuracoes.VersaoQRCodeNFCe == 3)
                        {
                            paramLinkQRCode += "|" + tpDest + "|" + ifDest;
                            paramLinkQRCode += "|" + Converter.ToRSASHA1(configuracoes.CertificadoDigital, paramLinkQRCode);
                        }
                        else
                        {
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

                            paramLinkQRCode += "|" + Converter.ToHexadecimal(digestValue.ToString()) + "|" + configuracoes.CSCIDToken.ToString();
                        }
                    }
                    else if (configuracoes.VersaoQRCodeNFCe <= 2)
                    {
                        paramLinkQRCode += "|" + configuracoes.CSCIDToken.ToString();
                    }

                    string qrCode;
                    if (configuracoes.VersaoQRCodeNFCe == 3)
                    {
                        qrCode = urlQrCode + "?p=" + paramLinkQRCode.Trim();
                    }
                    else
                    {
                        var hashQRCode = Converter.ToSHA1HashData(paramLinkQRCode.Trim() + configuracoes.CSC, true);
                        qrCode = urlQrCode + "?p=" + paramLinkQRCode.Trim() + "|" + hashQRCode.Trim();
                    }

                    var nodeInfNFe = (XmlNode)elementInfNFe;

                    AdicionarGrupoSuplementar(
                        conteudoXml,
                        nodeNFe,
                        nodeInfNFe,
                        "infNFeSupl",
                        new KeyValuePair<string, string>("qrCode", qrCode),
                        new KeyValuePair<string, string>("urlChave", urlChave));
                }
            }
        }

        /// <summary>
        /// Monta e inclui o grupo suplementar <c>infNFComSupl</c> com a tag de QRCode para NFCom.
        /// </summary>
        /// <param name="conteudoXml">Documento XML da NFCom já carregado em memória.</param>
        /// <param name="configuracoes">Configurações do serviço (ambiente, URLs e certificado digital).</param>
        /// <remarks>
        /// Quando a emissão exige assinatura adicional no parâmetro do QRCode,
        /// o certificado digital configurado é utilizado para gerar o valor de <c>sign</c>.
        /// </remarks>
        /// <exception cref="Exception">
        /// Lançada quando alguma tag obrigatória para cálculo da chave/QRCode não é localizada.
        /// </exception>
        public static void MontarQrCodeNFCom(XmlDocument conteudoXml, Configuracao configuracoes)
        {
            if (conteudoXml.GetElementsByTagName("NFCom").Count <= 0)
            {
                throw new Exception("A tag obrigatória <NFCom> não foi localizada no XML.");
            }

            var elementNFCom = (XmlElement)conteudoXml.GetElementsByTagName("NFCom")[0];

            if (elementNFCom.GetElementsByTagName("infNFComSupl").Count <= 0)
            {
                if (elementNFCom.GetElementsByTagName("infNFCom").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <infNFCom>, do grupo de tag <NFCom>, não foi localizada no XML.");
                }

                var elementInfNFCom = (XmlElement)elementNFCom.GetElementsByTagName("infNFCom")[0];

                if (elementInfNFCom.GetElementsByTagName("ide").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <ide>, do grupo de tag <NFCom><infNFCom>, não foi localizada no XML.");
                }

                var elementIde = (XmlElement)elementInfNFCom.GetElementsByTagName("ide")[0];

                var tpAmb = (TipoAmbiente)Convert.ToInt32(elementIde.GetElementsByTagName("tpAmb")[0].InnerText);
                var cUF = (UFBrasil)Convert.ToInt32(elementIde.GetElementsByTagName("cUF")[0].InnerText);
                var dhEmi = DateTimeOffset.Parse(elementIde.GetElementsByTagName("dhEmi")[0].InnerText);
                var serie = elementIde.GetElementsByTagName("serie")[0].InnerText;
                var nNF = elementIde.GetElementsByTagName("nNF")[0].InnerText;
                var tpEmis = (TipoEmissao)Convert.ToInt32(elementIde.GetElementsByTagName("tpEmis")[0].InnerText);
                var nSiteAutoriz = elementIde.GetElementsByTagName("nSiteAutoriz")[0].InnerText;
                var cNF = elementIde.GetElementsByTagName("cNF")[0].InnerText;
                var mod = elementIde.GetElementsByTagName("mod")[0].InnerText;

                if (elementInfNFCom.GetElementsByTagName("emit").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <emit>, do grupo de tag <NFCom><infNFCom>, não foi localizada no XML.");
                }

                var elementEmit = (XmlElement)elementInfNFCom.GetElementsByTagName("emit")[0];

                if (elementEmit.GetElementsByTagName("CNPJ").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <CNPJ>, do grupo de tag <NFCom><infNFCom><emit>, não foi localizada no XML.");
                }

                var cnpjEmit = elementEmit.GetElementsByTagName("CNPJ")[0].InnerText;

                var conteudoChaveDFe = new XMLUtility.ConteudoChaveDFe
                {
                    UFEmissor = (UFBrasil)Convert.ToInt32(cUF),
                    AnoEmissao = dhEmi.ToString("yy"),
                    MesEmissao = dhEmi.ToString("MM"),
                    CNPJCPFEmissor = cnpjEmit.PadLeft(14, '0'),
                    Modelo = (ModeloDFe)Convert.ToInt32(mod),
                    Serie = Convert.ToInt32(serie),
                    NumeroDoctoFiscal = Convert.ToInt32(nNF),
                    TipoEmissao = (TipoEmissao)(int)tpEmis,
                    NSiteAutoriz = nSiteAutoriz,
                    CodigoNumerico = cNF
                };

                var chave = XMLUtility.MontarChaveNFCom(ref conteudoChaveDFe);

                var urlQrCode = configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? configuracoes.UrlQrCodeHomologacao : configuracoes.UrlQrCodeProducao;
                var paramLinkQRCode = urlQrCode + "?chNFCom=" + chave + "&tpAmb=" + ((int)tpAmb).ToString();

                if ((int)tpEmis == 2)
                {
                    paramLinkQRCode += "&sign=" + Converter.ToRSASHA1(configuracoes.CertificadoDigital, chave);
                }

                var nodeNFCom = conteudoXml.GetElementsByTagName("NFCom")[0];
                var nodeInfNFCom = (XmlNode)elementInfNFCom;

                AdicionarGrupoSuplementar(
                    conteudoXml,
                    nodeNFCom,
                    nodeInfNFCom,
                    "infNFComSupl",
                    new KeyValuePair<string, string>("qrCodNFCom", paramLinkQRCode.Trim()));
            }
        }

        /// <summary>
        /// Monta e inclui o grupo suplementar <c>infCTeSupl</c> para CT-e (modelo tradicional).
        /// </summary>
        /// <param name="conteudoXml">Documento XML do CT-e.</param>
        /// <param name="configuracoes">Configurações do serviço utilizadas na montagem do QRCode.</param>
        /// <exception cref="Exception">Lançada quando faltam tags obrigatórias do XML para montagem do QRCode.</exception>
        public static void MontarQrCodeCTe(XmlDocument conteudoXml, Configuracao configuracoes) => MontarQrCodeCTeBase(conteudoXml, configuracoes, "CTe");

        /// <summary>
        /// Monta e inclui o grupo suplementar <c>infCTeSupl</c> para CT-e Simplificado.
        /// </summary>
        /// <param name="conteudoXml">Documento XML do CT-e Simplificado.</param>
        /// <param name="configuracoes">Configurações do serviço utilizadas na montagem do QRCode.</param>
        /// <exception cref="Exception">Lançada quando faltam tags obrigatórias do XML para montagem do QRCode.</exception>
        public static void MontarQrCodeCTeSimp(XmlDocument conteudoXml, Configuracao configuracoes) => MontarQrCodeCTeBase(conteudoXml, configuracoes, "CTeSimp");

        /// <summary>
        /// Monta e inclui o grupo suplementar <c>infCTeSupl</c> para CT-e OS.
        /// </summary>
        /// <param name="conteudoXml">Documento XML do CT-e OS.</param>
        /// <param name="configuracoes">Configurações do serviço utilizadas na montagem do QRCode.</param>
        /// <exception cref="Exception">Lançada quando faltam tags obrigatórias do XML para montagem do QRCode.</exception>
        public static void MontarQrCodeCTeOS(XmlDocument conteudoXml, Configuracao configuracoes) => MontarQrCodeCTeBase(conteudoXml, configuracoes, "CTeOS");

        /// <summary>
        /// Monta e inclui o grupo suplementar <c>infMDFeSupl</c> com a tag de QRCode para MDF-e.
        /// </summary>
        /// <param name="conteudoXml">Documento XML do MDF-e.</param>
        /// <param name="configuracoes">Configurações do serviço (ambiente, URLs e certificado digital).</param>
        /// <remarks>
        /// Em contingência com exigência de assinatura no parâmetro do QRCode,
        /// o valor <c>sign</c> é calculado com o certificado configurado.
        /// </remarks>
        /// <exception cref="Exception">
        /// Lançada quando alguma tag obrigatória para cálculo da chave/QRCode não é localizada.
        /// </exception>
        public static void MontarQrCodeMDFe(XmlDocument conteudoXml, Configuracao configuracoes)
        {
            if (conteudoXml.GetElementsByTagName("MDFe").Count <= 0)
            {
                throw new Exception("A tag obrigatória <MDFe> não foi localizada no XML.");
            }
            var elementMDFe = (XmlElement)conteudoXml.GetElementsByTagName("MDFe")[0];

            if (elementMDFe.GetElementsByTagName("infMDFeSupl").Count <= 0)
            {
                if (elementMDFe.GetElementsByTagName("infMDFe").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <infMDFe>, do grupo de tag <MDFe>, não foi localizada no XML.");
                }
                var elementInfMDFe = (XmlElement)elementMDFe.GetElementsByTagName("infMDFe")[0];

                if (elementInfMDFe.GetElementsByTagName("ide").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <ide>, do grupo de tag <MDFe><infMDFe>, não foi localizada no XML.");
                }
                var elementIde = (XmlElement)elementInfMDFe.GetElementsByTagName("ide")[0];

                var tpAmb = (TipoAmbiente)Convert.ToInt32(elementIde.GetElementsByTagName("tpAmb")[0].InnerText);
                var cUF = elementIde.GetElementsByTagName("cUF")[0].InnerText;
                var dhEmi = DateTimeOffset.Parse(elementIde.GetElementsByTagName("dhEmi")[0].InnerText);
                var serie = elementIde.GetElementsByTagName("serie")[0].InnerText;
                var nMDF = elementIde.GetElementsByTagName("nMDF")[0].InnerText;
                var cMDF = elementIde.GetElementsByTagName("cMDF")[0].InnerText;
                var tpEmis = (TipoEmissao)Convert.ToInt32(elementIde.GetElementsByTagName("tpEmis")[0].InnerText);
                var mod = elementIde.GetElementsByTagName("mod")[0].InnerText;

                if (elementInfMDFe.GetElementsByTagName("emit").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <emit>, do grupo de tag <MDFe><infNFe>, não foi localizada no XML.");
                }
                var elementEmit = (XmlElement)elementMDFe.GetElementsByTagName("emit")[0];

                var CNPJEmit = string.Empty;
                var CPFEmit = string.Empty;
                if (elementEmit.GetElementsByTagName("CNPJ").Count <= 0)
                {
                    if (elementEmit.GetElementsByTagName("CPF").Count <= 0)
                    {
                        throw new Exception("A tag obrigatória <CNPJ> ou <CPF>, do grupo de tag <MDFe><infMDFe><emit>, não foi localizada no XML.");
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
                    NumeroDoctoFiscal = Convert.ToInt32(nMDF),
                    TipoEmissao = (TipoEmissao)(int)tpEmis,
                    CodigoNumerico = cMDF
                };

                var chave = XMLUtility.MontarChaveMDFe(ref conteudoChaveDFe);

                var urlQrCode = configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? configuracoes.UrlQrCodeHomologacao : configuracoes.UrlQrCodeProducao;
                var paramLinkQRCode = urlQrCode +
                    "?chMDFe=" + chave +
                    "&tpAmb=" + ((int)tpAmb).ToString();

                if (tpEmis == TipoEmissao.ContingenciaFSIA)
                {
                    paramLinkQRCode += "&sign=" + Converter.ToRSASHA1(configuracoes.CertificadoDigital, chave);
                }

                var nodeMDFe = conteudoXml.GetElementsByTagName("MDFe")[0];
                var nodeInfMDFe = (XmlNode)elementInfMDFe;

                AdicionarGrupoSuplementar(
                    conteudoXml,
                    nodeMDFe,
                    nodeInfMDFe,
                    "infMDFeSupl",
                    new KeyValuePair<string, string>("qrCodMDFe", paramLinkQRCode.Trim()));
            }
        }

        /// <summary>
        /// Monta e inclui o grupo suplementar <c>infNF3eSupl</c> com a tag de QRCode para NF3e.
        /// </summary>
        /// <param name="conteudoXml">Documento XML da NF3e.</param>
        /// <param name="configuracoes">Configurações do serviço (ambiente, URLs e certificado digital).</param>
        /// <exception cref="Exception">
        /// Lançada quando alguma tag obrigatória para cálculo da chave/QRCode não é localizada.
        /// </exception>
        public static void MontarQrCodeNF3e(XmlDocument conteudoXml, Configuracao configuracoes)
        {
            if (conteudoXml.GetElementsByTagName("NF3e").Count <= 0)
            {
                throw new Exception("A tag obrigatória <NF3e> não foi localizada no XML.");
            }

            var elementNF3e = (XmlElement)conteudoXml.GetElementsByTagName("NF3e")[0];

            if (elementNF3e.GetElementsByTagName("infNF3eSupl").Count <= 0)
            {
                if (elementNF3e.GetElementsByTagName("infNF3e").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <infNF3e>, do grupo de tag <NF3e>, não foi localizada no XML.");
                }

                var elementInfNF3e = (XmlElement)elementNF3e.GetElementsByTagName("infNF3e")[0];

                if (elementInfNF3e.GetElementsByTagName("ide").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <ide>, do grupo de tag <NF3e><infNF3e>, não foi localizada no XML.");
                }

                var elementIde = (XmlElement)elementInfNF3e.GetElementsByTagName("ide")[0];

                var tpAmb = (TipoAmbiente)Convert.ToInt32(elementIde.GetElementsByTagName("tpAmb")[0].InnerText);
                var cUF = (UFBrasil)Convert.ToInt32(elementIde.GetElementsByTagName("cUF")[0].InnerText);
                var dhEmi = DateTimeOffset.Parse(elementIde.GetElementsByTagName("dhEmi")[0].InnerText);
                var serie = elementIde.GetElementsByTagName("serie")[0].InnerText;
                var nNF = elementIde.GetElementsByTagName("nNF")[0].InnerText;
                var tpEmis = (TipoEmissao)Convert.ToInt32(elementIde.GetElementsByTagName("tpEmis")[0].InnerText);
                var nSiteAutoriz = elementIde.GetElementsByTagName("nSiteAutoriz")[0].InnerText;
                var cNF = elementIde.GetElementsByTagName("cNF")[0].InnerText;
                var mod = elementIde.GetElementsByTagName("mod")[0].InnerText;

                if (elementInfNF3e.GetElementsByTagName("emit").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <emit>, do grupo de tag <NF3e><infNF3e>, não foi localizada no XML.");
                }

                var elementEmit = (XmlElement)elementInfNF3e.GetElementsByTagName("emit")[0];

                if (elementEmit.GetElementsByTagName("CNPJ").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <CNPJ>, do grupo de tag <NF3e><infNF3e><emit>, não foi localizada no XML.");
                }

                var cnpjEmit = elementEmit.GetElementsByTagName("CNPJ")[0].InnerText;

                var conteudoChaveDFe = new XMLUtility.ConteudoChaveDFe
                {
                    UFEmissor = (UFBrasil)Convert.ToInt32(cUF),
                    AnoEmissao = dhEmi.ToString("yy"),
                    MesEmissao = dhEmi.ToString("MM"),
                    CNPJCPFEmissor = cnpjEmit.PadLeft(14, '0'),
                    Modelo = (ModeloDFe)Convert.ToInt32(mod),
                    Serie = Convert.ToInt32(serie),
                    NumeroDoctoFiscal = Convert.ToInt32(nNF),
                    TipoEmissao = (TipoEmissao)(int)tpEmis,
                    NSiteAutoriz = nSiteAutoriz,
                    CodigoNumerico = cNF
                };

                var chave = XMLUtility.MontarChaveNF3e(ref conteudoChaveDFe);

                var urlQrCode = configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? configuracoes.UrlQrCodeHomologacao : configuracoes.UrlQrCodeProducao;
                var paramLinkQRCode = urlQrCode + "?chNF3e=" + chave + "&tpAmb=" + ((int)tpAmb).ToString();

                if (tpEmis == TipoEmissao.ContingenciaOffLine)
                {
                    paramLinkQRCode += "&sign=" + Converter.ToRSASHA1(configuracoes.CertificadoDigital, chave);
                }

                var nodeNF3e = conteudoXml.GetElementsByTagName("NF3e")[0];
                var nodeInfNF3e = (XmlNode)elementInfNF3e;

                AdicionarGrupoSuplementar(
                    conteudoXml,
                    nodeNF3e,
                    nodeInfNF3e,
                    "infNF3eSupl",
                    new KeyValuePair<string, string>("qrCodNF3e", paramLinkQRCode.Trim()));
            }
        }

        /// <summary>
        /// Implementação base para montagem de QRCode da família CT-e (<c>CTe</c>, <c>CTeSimp</c> e <c>CTeOS</c>).
        /// </summary>
        /// <param name="conteudoXml">Documento XML do tipo CT-e correspondente.</param>
        /// <param name="configuracoes">Configurações do serviço para composição do QRCode.</param>
        /// <param name="nomeTagDocumento">
        /// Nome da tag raiz do documento a processar
        /// (por exemplo: <c>CTe</c>, <c>CTeSimp</c> ou <c>CTeOS</c>).
        /// </param>
        /// <exception cref="Exception">
        /// Lançada quando o XML não possui a estrutura mínima esperada para o tipo informado.
        /// </exception>
        private static void MontarQrCodeCTeBase(XmlDocument conteudoXml, Configuracao configuracoes, string nomeTagDocumento)
        {
            if (conteudoXml.GetElementsByTagName(nomeTagDocumento).Count <= 0)
            {
                throw new Exception($"A tag obrigatória <{nomeTagDocumento}> não foi localizada no XML.");
            }
            var elementCTe = (XmlElement)conteudoXml.GetElementsByTagName(nomeTagDocumento)[0];

            if (elementCTe.GetElementsByTagName("infCTeSupl").Count <= 0)
            {
                if (elementCTe.GetElementsByTagName("infCte").Count <= 0)
                {
                    throw new Exception($"A tag obrigatória <infCte>, do grupo de tag <{nomeTagDocumento}>, não foi localizada no XML.");
                }
                var elementInfCte = (XmlElement)elementCTe.GetElementsByTagName("infCte")[0];

                if (elementInfCte.GetElementsByTagName("ide").Count <= 0)
                {
                    throw new Exception($"A tag obrigatória <ide>, do grupo de tag <{nomeTagDocumento}><infCte>, não foi localizada no XML.");
                }
                var elementIde = (XmlElement)elementInfCte.GetElementsByTagName("ide")[0];

                var tpAmb = (TipoAmbiente)Convert.ToInt32(elementIde.GetElementsByTagName("tpAmb")[0].InnerText);
                var cUF = elementIde.GetElementsByTagName("cUF")[0].InnerText;
                var dhEmi = DateTimeOffset.Parse(elementIde.GetElementsByTagName("dhEmi")[0].InnerText);
                var serie = elementIde.GetElementsByTagName("serie")[0].InnerText;
                var nCT = elementIde.GetElementsByTagName("nCT")[0].InnerText;
                var cCT = elementIde.GetElementsByTagName("cCT")[0].InnerText;
                var tpEmis = (TipoEmissao)Convert.ToInt32(elementIde.GetElementsByTagName("tpEmis")[0].InnerText);
                var mod = elementIde.GetElementsByTagName("mod")[0].InnerText;

                if (elementInfCte.GetElementsByTagName("emit").Count <= 0)
                {
                    throw new Exception($"A tag obrigatória <emit>, do grupo de tag <{nomeTagDocumento}><infCte>, não foi localizada no XML.");
                }
                var elementEmit = (XmlElement)elementInfCte.GetElementsByTagName("emit")[0];

                var CNPJEmit = string.Empty;
                var CPFEmit = string.Empty;
                if (elementEmit.GetElementsByTagName("CNPJ").Count <= 0)
                {
                    if (elementEmit.GetElementsByTagName("CPF").Count <= 0)
                    {
                        throw new Exception($"A tag obrigatória <CNPJ> ou <CPF>, do grupo de tag <{nomeTagDocumento}><infCte><emit>, não foi localizada no XML.");
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
                    NumeroDoctoFiscal = Convert.ToInt32(nCT),
                    TipoEmissao = (TipoEmissao)(int)tpEmis,
                    CodigoNumerico = cCT
                };

                var chave = XMLUtility.MontarChaveCTe(ref conteudoChaveDFe);

                var urlQrCode = configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? configuracoes.UrlQrCodeHomologacao : configuracoes.UrlQrCodeProducao;
                var paramLinkQRCode = urlQrCode +
                    "?chCTe=" + chave +
                    "&tpAmb=" + ((int)tpAmb).ToString();

                if (tpEmis == TipoEmissao.ContingenciaEPEC || tpEmis == TipoEmissao.ContingenciaFSDA)
                {
                    paramLinkQRCode += "&sign=" + Converter.ToRSASHA1(configuracoes.CertificadoDigital, chave);
                }

                var nodeCTe = conteudoXml.GetElementsByTagName(nomeTagDocumento)[0];
                var nodeInfCTe = (XmlNode)elementInfCte;

                AdicionarGrupoSuplementar(
                    conteudoXml,
                    nodeCTe,
                    nodeInfCTe,
                    "infCTeSupl",
                    new KeyValuePair<string, string>("qrCodCTe", paramLinkQRCode.Trim()));
            }
        }

        /// <summary>
        /// Cria e adiciona um grupo suplementar no XML do documento logo após um nó de referência.
        /// </summary>
        /// <param name="conteudoXml">Documento XML onde o grupo será criado e inserido.</param>
        /// <param name="nodePai">Nó pai que receberá o grupo suplementar.</param>
        /// <param name="nodeReferencia">Nó de referência usado para posicionar o grupo via <c>InsertAfter</c>.</param>
        /// <param name="nomeTagSuplementar">Nome da tag do grupo suplementar a ser criado (ex.: <c>infNFeSupl</c>).</param>
        /// <param name="tags">Coleção de pares chave/valor representando as tags filhas do grupo suplementar.</param>
        /// <remarks>
        /// O namespace padrão do grupo e de suas tags filhas é herdado do <paramref name="nodePai"/>.
        /// </remarks>
        public static void AdicionarGrupoSuplementar(XmlDocument conteudoXml, XmlNode nodePai, XmlNode nodeReferencia, string nomeTagSuplementar, params KeyValuePair<string, string>[] tags)
        {
            var namespaceURI = nodePai.GetNamespaceOfPrefix("");
            var grupoSuplementarNode = conteudoXml.CreateElement(nomeTagSuplementar, namespaceURI);

            foreach (var tag in tags)
            {
                var node = conteudoXml.CreateElement(tag.Key, namespaceURI);
                node.InnerText = tag.Value;
                grupoSuplementarNode.AppendChild(node);
            }

            nodePai.AppendChild(grupoSuplementarNode);
            nodePai.InsertAfter(grupoSuplementarNode, nodeReferencia);
        }
    }
}
