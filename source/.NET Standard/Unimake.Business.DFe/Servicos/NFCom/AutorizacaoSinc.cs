#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFCom;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFCom
{
    /// <summary>
    /// Enviar o XML da NFCom para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFCom.AutorizacaoSinc")]
    [ComVisible(true)]
#endif
    public class AutorizacaoSinc : ServicoBase, IInteropService<Xml.NFCom.NFCom>
    {
        #region Private Fields

        private Xml.NFCom.NFCom _NFCom;
        private readonly Dictionary<string, NFComProc> NFComProcs = new Dictionary<string, NFComProc>();

        #endregion Private Fields

        #region Protected Properties

        /// <summary>
        /// Objeto XML da NFCom
        /// </summary>
        public Xml.NFCom.NFCom NFCom
        {
            get => _NFCom ?? (_NFCom = new Xml.NFCom.NFCom().LerXML<Xml.NFCom.NFCom>(ConteudoXML));
            protected set => _NFCom = value;
        }

        #endregion Protected Properties

        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.NFComAutorizacaoSinc;

                if (ConteudoXML.GetElementsByTagName("NFCom").Count > 0)
                {
                    var tagNFCom = (XmlElement)ConteudoXML.GetElementsByTagName("NFCom")[0];
                    if (tagNFCom.GetElementsByTagName("infNFCom").Count > 0)
                    {
                        var tagInfNFCom = (XmlElement)ConteudoXML.GetElementsByTagName("infNFCom")[0];

                        if (tagInfNFCom.GetAttribute("versao").Length > 0)
                        {
                            Configuracoes.SchemaVersao = tagInfNFCom.GetAttribute("versao");
                        }
                        else
                        {
                            throw new Exception("O atributo obrigatório \"versao\" da tag <infNFCom>, do grupo de tag <NFCom>, não foi localizado no XML.");
                        }

                        if (tagInfNFCom.GetElementsByTagName("ide").Count > 0)
                        {
                            var tagIde = (XmlElement)tagInfNFCom.GetElementsByTagName("ide")[0];

                            if (tagIde.GetElementsByTagName("cUF").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <cUF>, do grupo de tag <NFCom><infNFCom><ide>, não foi localizada no XML.");
                            }
                            else if (tagIde.GetElementsByTagName("mod").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <mod>, do grupo de tag <NFCom><infNFCom><ide>, não foi localizada no XML.");
                            }
                            else if (tagIde.GetElementsByTagName("tpEmis").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <tpEmis>, do grupo de tag <NFCom><infNFCom><ide>, não foi localizada no XML.");
                            }
                            else if (tagIde.GetElementsByTagName("tpAmb").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <tpAmb>, do grupo de tag <NFCom><infNFCom><ide>, não foi localizada no XML.");
                            }

                            Configuracoes.CodigoUF = Convert.ToInt32(tagIde.GetElementsByTagName("cUF")[0].InnerText);
                            Configuracoes.Modelo = (ModeloDFe)Convert.ToInt32(tagIde.GetElementsByTagName("mod")[0].InnerText);
                            Configuracoes.TipoEmissao = (TipoEmissao)Convert.ToInt32(tagIde.GetElementsByTagName("tpEmis")[0].InnerText);
                            Configuracoes.TipoAmbiente = (TipoAmbiente)Convert.ToInt32(tagIde.GetElementsByTagName("tpAmb")[0].InnerText);
                        }
                        else
                        {
                            throw new Exception("A tag obrigatória <ide>, do grupo de tag <NFCom><infNFCom>, não foi localizada no XML.");
                        }
                    }
                    else
                    {
                        throw new Exception("A tag obrigatória <infNFCom>, do grupo de tag <NFCom>, não foi localizada no XML.");
                    }
                }
                else
                {
                    throw new Exception("A tag obrigatória <NFCom> não foi localizada no XML.");
                }

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Montar o QRCode da NFCom
        /// </summary>
        private void MontarQrCode()
        {
            if (ConteudoXML.GetElementsByTagName("NFCom").Count <= 0)
            {
                throw new Exception("A tag obrigatória <NFCom> não foi localizada no XML.");
            }

            var elementNFCom = (XmlElement)ConteudoXML.GetElementsByTagName("NFCom")[0];

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

                var cnpjEmit = string.Empty;

                if (elementEmit.GetElementsByTagName("CNPJ").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <CNPJ>, do grupo de tag <NFCom><infNFCom><emit>, não foi localizada no XML.");
                }

                cnpjEmit = elementEmit.GetElementsByTagName("CNPJ")[0].InnerText;

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

                var urlQrCode = Configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? Configuracoes.UrlQrCodeHomologacao : Configuracoes.UrlQrCodeProducao;
                var paramLinkQRCode = urlQrCode + "?chNFCom=" + chave + "&tpAmb=" + ((int)tpAmb).ToString();

                if (tpEmis == TipoEmissao.ContingenciaOffLine)
                {
                    paramLinkQRCode += "&sign=" + Converter.ToRSASHA1(Configuracoes.CertificadoDigital, chave);
                }

                var nodeNFCom = ConteudoXML.GetElementsByTagName("NFCom")[0];

                var namespaceURI = nodeNFCom.GetNamespaceOfPrefix("");
                XmlNode infNFComSuplNode = ConteudoXML.CreateElement("infNFComSupl", namespaceURI);
                XmlNode qrCodeNFComNode = ConteudoXML.CreateElement("qrCodNFCom", namespaceURI);
                qrCodeNFComNode.InnerText = paramLinkQRCode.Trim();
                infNFComSuplNode.AppendChild(qrCodeNFComNode);
                nodeNFCom.AppendChild(infNFComSuplNode);
                var nodeInfNFCom = (XmlNode)elementInfNFCom;
                nodeNFCom.InsertAfter(infNFComSuplNode, nodeInfNFCom);
            }
        }

        /// <summary>
        /// Efetuar um Ajustse no XML da NFCom logo depois de assinado
        /// </summary>
        protected override void AjustarXMLAposAssinado()
        {
            MontarQrCode();
            base.AjustarXMLAposAssinado();
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Lista com o conteúdo retornado das consultas situação do NFComs enviadas
        /// </summary>
        public List<RetConsSitNFCom> RetConsSitNF3 = new List<RetConsSitNFCom>();

        /// <summary>
        /// Propriedade contendo o XML da NFCom com o protocolo de autorização anexado - Funciona para envio Assíncrono ou Síncrono
        /// </summary>
        public Dictionary<string, NFComProc> NFComProcResults
        {
            get
            {
                if (Result.ProtNFCom != null)
                {
                    if (NFComProcs.ContainsKey(NFCom.InfNFCom.Chave))
                    {
                        NFComProcs[NFCom.InfNFCom.Chave].ProtNFCom = Result.ProtNFCom;
                    }
                    else
                    {
                        NFComProcs.Add(NFCom.InfNFCom.Chave, new NFComProc
                        {
                            Versao = NFCom.InfNFCom.Versao,
                            NFCom = NFCom,
                            ProtNFCom = Result.ProtNFCom
                        });
                    }
                }
                else
                {
                    if (RetConsSitNF3.Count <= 0)
                    {
                        throw new Exception("Defina o conteúdo da Propriedade RetConsSitNF3, sem a definição dela não é possível obter o conteúdo da NFComProcResults.");
                    }

                    ProtNFCom protNFCom = null;

                    #region Resultado do envio do NFCom através da consulta situação

                    foreach (var item in RetConsSitNF3)
                    {
                        if (item != null && item.ProtNFCom != null)
                        {
                            if (item.ProtNFCom.InfProt.ChNFCom == NFCom.InfNFCom.Chave)
                            {
                                switch (item.ProtNFCom.InfProt.CStat)
                                {
                                    case 100: //NFCom autorizada
                                        protNFCom = item.ProtNFCom;
                                        break;
                                }
                            }
                        }
                    }

                    if (NFComProcs.ContainsKey(NFCom.InfNFCom.Chave))
                    {
                        NFComProcs[NFCom.InfNFCom.Chave].ProtNFCom = protNFCom;
                    }

                    else
                    {
                        NFComProcs.Add(NFCom.InfNFCom.Chave, new NFComProc
                        {
                            Versao = NFCom.InfNFCom.Versao,
                            NFCom = NFCom,
                            ProtNFCom = protNFCom
                        });
                    }

                    #endregion Resultado do envio do NFCom através da consulta situação
                }

                return NFComProcs;
            }
        }

#if INTEROP

        /// <summary>
        /// Recupera o XML de distribuição do NFCom no formato string
        /// </summary>
        /// <param name="chaveDFe">Chave do NFCom que é para retornar o XML de distribuição</param>
        /// <returns>XML de distribuição do NFCom</returns>
        public string GetNFComProcResults(string chaveDFe)
        {
            var retornar = "";
            if (NFComProcResults.Count > 0)
            {
                retornar = NFComProcResults[chaveDFe].GerarXML().OuterXml;
            }

            return retornar;
        }

#endif

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetNFCom Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetNFCom>(RetornoWSString);
                }

                return new RetNFCom
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        #endregion Public Properties


        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public AutorizacaoSinc() : base() => NFComProcs.Clear();

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="NFCom">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public AutorizacaoSinc(Xml.NFCom.NFCom NFCom, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(NFCom?.GerarXML() ?? throw new ArgumentNullException(nameof(NFCom)), configuracao);

            NFCom = NFCom.LerXML<Xml.NFCom.NFCom>(ConteudoXML);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        /// <exception cref="ArgumentNullException"></exception>
        public AutorizacaoSinc(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            Inicializar(doc, configuracao);

            #region Limpar a assinatura e QRCode do objeto para recriar e atualizar o ConteudoXML. Isso garante que a propriedade e o objeto tenham assinaturas iguais, evitando discrepâncias. Autor: Wandrey Data: 10/06/2024

            //Remover a assinatura e QRCode para forçar criar novamente
            NFCom = NFCom.LerXML<Xml.NFCom.NFCom>(ConteudoXML);
            NFCom.Signature = null;
            NFCom.InfNFComSupl = null;

            //Gerar o XML novamente com base no objeto
            ConteudoXML = NFCom.GerarXML();

            //Forçar assinar e criar o QRCode novamente
            _ = ConteudoXMLAssinado;

            //Atualizar o objeto novamente com o XML já assinado e com QRCode
            NFCom = NFCom.LerXML<Xml.NFCom.NFCom>(ConteudoXML);

            #endregion Limpar a assinatura e QRCode do objeto para recriar e atualizar o ConteudoXML. Isso garante que a propriedade e o objeto tenham assinaturas iguais, evitando discrepâncias. Autor: Wandrey Data: 10/06/2024
        }

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            base.Executar();
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="NFCom">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        public void Executar(Xml.NFCom.NFCom NFCom, Configuracao configuracao)
        {
            try
            {
                Inicializar(NFCom?.GerarXML() ?? throw new ArgumentNullException(nameof(NFCom)), configuracao);

                Executar();
            }
            catch (ValidarXMLException ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
            catch (CertificadoDigitalException ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Definir o objeto contendo o XML a ser enviado e configuração de conexão e envio do XML para web-service
        /// </summary>
        /// <param name="NFCom">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(Xml.NFCom.NFCom NFCom, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(NFCom?.GerarXML() ?? throw new ArgumentNullException(nameof(NFCom)), configuracao);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

#endif

        /// <summary>
        /// Gravar o XML de distribuição em uma pasta no HD
        /// </summary>
        /// <param name="pasta">Pasta onde deve ser gravado o XML</param>
#if INTEROP
        [ComVisible(true)]
#endif
        public void GravarXmlDistribuicao(string pasta)
        {
            try
            {
                foreach (var item in NFComProcResults)
                {
                    if (item.Value.ProtNFCom != null)
                    {
                        GravarXmlDistribuicao(pasta, item.Value.NomeArquivoDistribuicao, item.Value.GerarXML().OuterXml);
                    }
                    else
                    {
                        throw new Exception("Não foi localizado no retorno da consulta o protocolo da chave, abaixo, para a elaboração do arquivo de distribuição. Verifique se a chave ou recibo consultado estão de acordo com a informada na sequencia:\r\n\r\n" + Format.ChaveNFComDFe(item.Key));
                    }
                }
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Grava o XML de distribuição no stream
        /// </summary>
        /// <param name="stream">Stream que vai receber o XML de distribuição</param>
#if INTEROP
        [ComVisible(false)]
#endif
        public void GravarXmlDistribuicao(System.IO.Stream stream)
        {
            try
            {
                foreach (var item in NFComProcResults)
                {
                    if (item.Value.ProtNFCom != null)
                    {
                        GravarXmlDistribuicao(stream, item.Value.GerarXML().OuterXml);
                    }
                    else
                    {
                        throw new Exception("Não foi localizado no retorno da consulta o protocolo da chave, abaixo, para a elaboração do arquivo de distribuição. Verifique se a chave ou recibo consultado estão de acordo com a informada na sequencia:\r\n\r\n" + Format.ChaveNFComDFe(item.Key));
                    }
                }
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }


        #endregion Public Methods

    }
}
