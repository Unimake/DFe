#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NF3e;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NF3e
{
    /// <summary>
    /// Enviar o XML da NF3e para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NF3e.AutorizacaoSinc")]
    [ComVisible(true)]
#endif
    public class AutorizacaoSinc : ServicoBase, IInteropService<Xml.NF3e.NF3e>
    {
        #region Private Fields

        private Xml.NF3e.NF3e _NF3e;
        private readonly Dictionary<string, NF3eProc> NF3eProcs = new Dictionary<string, NF3eProc>();

        #endregion Private Fields

        #region Protected Properties

        /// <summary>
        /// Objeto XML da NF3e
        /// </summary>
        public Xml.NF3e.NF3e NF3e
        {
            get => _NF3e ?? (_NF3e = new Xml.NF3e.NF3e().LerXML<Xml.NF3e.NF3e>(ConteudoXML));
            protected set => _NF3e = value;
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
                Configuracoes.Servico = Servico.NF3eAutorizacaoSinc;

                if (ConteudoXML.GetElementsByTagName("NF3e").Count > 0)
                {
                    var tagNF3e = (XmlElement)ConteudoXML.GetElementsByTagName("NF3e")[0];
                    if (tagNF3e.GetElementsByTagName("infNF3e").Count > 0)
                    {
                        var tagInfNF3e = (XmlElement)ConteudoXML.GetElementsByTagName("infNF3e")[0];

                        if (tagInfNF3e.GetAttribute("versao").Length > 0)
                        {
                            Configuracoes.SchemaVersao = tagInfNF3e.GetAttribute("versao");
                        }
                        else
                        {
                            throw new Exception("O atributo obrigatório \"versao\" da tag <infNF3e>, do grupo de tag <NF3e>, não foi localizado no XML.");
                        }

                        if (tagInfNF3e.GetElementsByTagName("ide").Count > 0)
                        {
                            var tagIde = (XmlElement)tagInfNF3e.GetElementsByTagName("ide")[0];

                            if (tagIde.GetElementsByTagName("cUF").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <cUF>, do grupo de tag <NF3e><infNF3e><ide>, não foi localizada no XML.");
                            }
                            else if (tagIde.GetElementsByTagName("mod").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <mod>, do grupo de tag <NF3e><infNF3e><ide>, não foi localizada no XML.");
                            }
                            else if (tagIde.GetElementsByTagName("tpEmis").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <tpEmis>, do grupo de tag <NF3e><infNF3e><ide>, não foi localizada no XML.");
                            }
                            else if (tagIde.GetElementsByTagName("tpAmb").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <tpAmb>, do grupo de tag <NF3e><infNF3e><ide>, não foi localizada no XML.");
                            }

                            Configuracoes.CodigoUF = Convert.ToInt32(tagIde.GetElementsByTagName("cUF")[0].InnerText);
                            Configuracoes.Modelo = (ModeloDFe)Convert.ToInt32(tagIde.GetElementsByTagName("mod")[0].InnerText);
                            Configuracoes.TipoEmissao = (TipoEmissao)Convert.ToInt32(tagIde.GetElementsByTagName("tpEmis")[0].InnerText);
                            Configuracoes.TipoAmbiente = (TipoAmbiente)Convert.ToInt32(tagIde.GetElementsByTagName("tpAmb")[0].InnerText);
                        }
                        else
                        {
                            throw new Exception("A tag obrigatória <ide>, do grupo de tag <NF3e><infNF3e>, não foi localizada no XML.");
                        }
                    }
                    else
                    {
                        throw new Exception("A tag obrigatória <infNF3e>, do grupo de tag <NF3e>, não foi localizada no XML.");
                    }
                }
                else
                {
                    throw new Exception("A tag obrigatória <NF3e> não foi localizada no XML.");
                }

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Montar o QRCode da NF3e
        /// </summary>
        private void MontarQrCode()
        {
            if (ConteudoXML.GetElementsByTagName("NF3e").Count <= 0)
            {
                throw new Exception("A tag obrigatória <NF3e> não foi localizada no XML.");
            }

            var elementNF3e = (XmlElement)ConteudoXML.GetElementsByTagName("NF3e")[0];

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

                var cnpjEmit = string.Empty;

                if (elementEmit.GetElementsByTagName("CNPJ").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <CNPJ>, do grupo de tag <NF3e><infNF3e><emit>, não foi localizada no XML.");
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

                var chave = XMLUtility.MontarChaveNF3e(ref conteudoChaveDFe);

                var urlQrCode = (Configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? Configuracoes.UrlQrCodeHomologacao : Configuracoes.UrlQrCodeProducao);
                var paramLinkQRCode = urlQrCode + "?chNF3e=" + chave + "&tpAmb=" + ((int)tpAmb).ToString();

                if (tpEmis == TipoEmissao.ContingenciaOffLine)
                {
                    paramLinkQRCode += "&sign=" + Converter.ToRSASHA1(Configuracoes.CertificadoDigital, chave);
                }

                var nodeNF3e = ConteudoXML.GetElementsByTagName("NF3e")[0];

                var namespaceURI = nodeNF3e.GetNamespaceOfPrefix("");
                XmlNode infNF3eSuplNode = ConteudoXML.CreateElement("infNF3eSupl", namespaceURI);
                XmlNode qrCodeNF3eNode = ConteudoXML.CreateElement("qrCodNF3e", namespaceURI);
                qrCodeNF3eNode.InnerText = paramLinkQRCode.Trim();
                infNF3eSuplNode.AppendChild(qrCodeNF3eNode);
                nodeNF3e.AppendChild(infNF3eSuplNode);
                var nodeInfNF3e = (XmlNode)elementInfNF3e;
                nodeNF3e.InsertAfter(infNF3eSuplNode, nodeInfNF3e);
            }
        }

        /// <summary>
        /// Efetuar um Ajustse no XML da NF3e logo depois de assinado
        /// </summary>
        protected override void AjustarXMLAposAssinado()
        {
            MontarQrCode();
            base.AjustarXMLAposAssinado();
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Lista com o conteúdo retornado das consultas situação do NF3es enviadas
        /// </summary>
        public List<RetConsSitNF3e> RetConsSitNF3 = new List<RetConsSitNF3e>();

        /// <summary>
        /// Propriedade contendo o XML da NF3e com o protocolo de autorização anexado - Funciona para envio Assíncrono ou Síncrono
        /// </summary>
        public Dictionary<string, NF3eProc> NF3eProcResults
        {
            get
            {
                if (Result.ProtNF3e != null)
                {
                    if (NF3eProcs.ContainsKey(NF3e.InfNF3e.Chave))
                    {
                        NF3eProcs[NF3e.InfNF3e.Chave].ProtNF3e = Result.ProtNF3e;
                    }
                    else
                    {
                        NF3eProcs.Add(NF3e.InfNF3e.Chave, new NF3eProc
                        {
                            Versao = NF3e.InfNF3e.Versao,
                            NF3e = NF3e,
                            ProtNF3e = Result.ProtNF3e
                        });
                    }
                }
                else
                {
                    if (RetConsSitNF3.Count <= 0)
                    {
                        throw new Exception("Defina o conteúdo da Propriedade RetConsSitNF3, sem a definição dela não é possível obter o conteúdo da NF3eProcResults.");
                    }

                    ProtNF3e protNF3e = null;

                    #region Resultado do envio do NF3e através da consulta situação

                    foreach (var item in RetConsSitNF3)
                    {
                        if (item != null && item.ProtNF3e != null)
                        {
                            if (item.ProtNF3e.InfProt.ChNF3e == NF3e.InfNF3e.Chave)
                            {
                                switch (item.ProtNF3e.InfProt.CStat)
                                {
                                    case 100: //NF3e autorizada
                                        protNF3e = item.ProtNF3e;
                                        break;
                                }
                            }
                        }
                    }

                    if (NF3eProcs.ContainsKey(NF3e.InfNF3e.Chave))
                    {
                        NF3eProcs[NF3e.InfNF3e.Chave].ProtNF3e = protNF3e;
                    }

                    else
                    {
                        NF3eProcs.Add(NF3e.InfNF3e.Chave, new NF3eProc
                        {
                            Versao = NF3e.InfNF3e.Versao,
                            NF3e = NF3e,
                            ProtNF3e = protNF3e
                        });
                    }

                    #endregion Resultado do envio do NF3e através da consulta situação
                }

                return NF3eProcs;
            }
        }

#if INTEROP

        /// <summary>
        /// Recupera o XML de distribuição do NF3e no formato string
        /// </summary>
        /// <param name="chaveDFe">Chave do NF3e que é para retornar o XML de distribuição</param>
        /// <returns>XML de distribuição do NF3e</returns>
        public string GetNF3eProcResults(string chaveDFe)
        {
            var retornar = "";
            if (NF3eProcResults.Count > 0)
            {
                retornar = NF3eProcResults[chaveDFe].GerarXML().OuterXml;
            }

            return retornar;
        }

#endif

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetNF3e Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetNF3e>(RetornoWSString);
                }

                return new RetNF3e
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
        public AutorizacaoSinc() : base() => NF3eProcs.Clear();

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="nf3e">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public AutorizacaoSinc(Xml.NF3e.NF3e nf3e, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(nf3e?.GerarXML() ?? throw new ArgumentNullException(nameof(nf3e)), configuracao);

            NF3e = NF3e.LerXML<Xml.NF3e.NF3e>(ConteudoXML);
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
            NF3e = NF3e.LerXML<Xml.NF3e.NF3e>(ConteudoXML);
            NF3e.Signature = null;
            NF3e.InfNF3eSupl = null;

            //Gerar o XML novamente com base no objeto
            ConteudoXML = NF3e.GerarXML();

            //Forçar assinar e criar o QRCode novamente
            _ = ConteudoXMLAssinado;

            //Atualizar o objeto novamente com o XML já assinado e com QRCode
            NF3e = NF3e.LerXML<Xml.NF3e.NF3e>(ConteudoXML);

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
        /// <param name="nf3e">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        public void Executar(Xml.NF3e.NF3e nf3e, Configuracao configuracao)
        {
            try
            {
                Inicializar(nf3e?.GerarXML() ?? throw new ArgumentNullException(nameof(nf3e)), configuracao);

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
        /// <param name="nf3e">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(Xml.NF3e.NF3e nf3e, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(nf3e?.GerarXML() ?? throw new ArgumentNullException(nameof(nf3e)), configuracao);
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
                foreach (var item in NF3eProcResults)
                {
                    if (item.Value.ProtNF3e != null)
                    {
                        GravarXmlDistribuicao(pasta, item.Value.NomeArquivoDistribuicao, item.Value.GerarXML().OuterXml);
                    }
                    else
                    {
                        throw new Exception("Não foi localizado no retorno da consulta o protocolo da chave, abaixo, para a elaboração do arquivo de distribuição. Verifique se a chave ou recibo consultado estão de acordo com a informada na sequencia:\r\n\r\n" + Format.ChaveNF3eDFe(item.Key));
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
                foreach (var item in NF3eProcResults)
                {
                    if (item.Value.ProtNF3e != null)
                    {
                        GravarXmlDistribuicao(stream, item.Value.GerarXML().OuterXml);
                    }
                    else
                    {
                        throw new Exception("Não foi localizado no retorno da consulta o protocolo da chave, abaixo, para a elaboração do arquivo de distribuição. Verifique se a chave ou recibo consultado estão de acordo com a informada na sequencia:\r\n\r\n" + Format.ChaveNF3eDFe(item.Key));
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
