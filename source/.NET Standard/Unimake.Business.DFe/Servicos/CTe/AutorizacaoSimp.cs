#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CTe;
using Unimake.Business.DFe.Xml.CTeSimp;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CTe
{
    /// <summary>
    /// Envio do XML de CTeSimp para web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CTe.AutorizacaoSimp")]
    [ComVisible(true)]
#endif
    public class AutorizacaoSimp : ServicoBase, IInteropService<CTeSimp>
    {
        private void MontarQrCode()
        {
            if (ConteudoXML.GetElementsByTagName("CTeSimp").Count <= 0)
            {
                throw new Exception("A tag obrigatória <CTeSimp> não foi localizada no XML.");
            }
            var elementCTeSimp = (XmlElement)ConteudoXML.GetElementsByTagName("CTeSimp")[0];

            if (elementCTeSimp.GetElementsByTagName("infCTeSupl").Count <= 0)
            {
                if (elementCTeSimp.GetElementsByTagName("infCte").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <infCte>, do grupo de tag <CTeSimp>, não foi localizada no XML.");
                }
                var elementInfCte = (XmlElement)elementCTeSimp.GetElementsByTagName("infCte")[0];

                if (elementInfCte.GetElementsByTagName("ide").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <ide>, do grupo de tag <CTeSimp><infCte>, não foi localizada no XML.");
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
                    throw new Exception("A tag obrigatória <emit>, do grupo de tag <CTeSimp><infCte>, não foi localizada no XML.");
                }
                var elementEmit = (XmlElement)elementInfCte.GetElementsByTagName("emit")[0];

                var CNPJEmit = string.Empty;
                var CPFEmit = string.Empty;
                if (elementEmit.GetElementsByTagName("CNPJ").Count <= 0)
                {
                    if (elementEmit.GetElementsByTagName("CPF").Count <= 0)
                    {
                        throw new Exception("A tag obrigatória <CNPJ> ou <CPF>, do grupo de tag <CTeSimp><infCte><emit>, não foi localizada no XML.");
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

                var urlQrCode = (Configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? Configuracoes.UrlQrCodeHomologacao : Configuracoes.UrlQrCodeProducao);
                var paramLinkQRCode = urlQrCode +
                    "?chCTe=" + chave +
                    "&tpAmb=" + ((int)tpAmb).ToString();

                if (tpEmis == TipoEmissao.ContingenciaEPEC || tpEmis == TipoEmissao.ContingenciaFSDA)
                {
                    paramLinkQRCode += "&sign=" + Converter.ToRSASHA1(Configuracoes.CertificadoDigital, chave);
                }

                var nodeCTeSimp = ConteudoXML.GetElementsByTagName("CTeSimp")[0];

                var namespaceURI = nodeCTeSimp.GetNamespaceOfPrefix("");
                XmlNode infCTeSuplNode = ConteudoXML.CreateElement("infCTeSupl", namespaceURI);
                XmlNode qrCodCTeNode = ConteudoXML.CreateElement("qrCodCTe", namespaceURI);
                qrCodCTeNode.InnerText = paramLinkQRCode.Trim();
                infCTeSuplNode.AppendChild(qrCodCTeNode);
                nodeCTeSimp.AppendChild(infCTeSuplNode);
                var nodeInfCTe = (XmlNode)elementInfCte;
                nodeCTeSimp.InsertAfter(infCTeSuplNode, nodeInfCTe);
            }
        }

        private CTeSimp _cteSimp;
        private readonly Dictionary<string, CteSimpProc> CteSimpProcs = new Dictionary<string, CteSimpProc>();

        /// <summary>
        /// Objeto do XML do CTe Simplificado
        /// </summary>
        public CTeSimp CTeSimp
        {
            get => _cteSimp ?? (_cteSimp = new CTeSimp().LerXML<CTeSimp>(ConteudoXML));
            protected set => _cteSimp = value;
        }

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.CTeAutorizacaoSimp;

                if (ConteudoXML.GetElementsByTagName("CTeSimp").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <CTeSimp> não foi localizada no XML.");
                }
                var elementCTeSimp = (XmlElement)ConteudoXML.GetElementsByTagName("CTeSimp")[0];

                if (elementCTeSimp.GetElementsByTagName("infCte").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <infCte>, do grupo de tag <CTeSimp>, não foi localizada no XML.");
                }
                var elementInfCte = (XmlElement)elementCTeSimp.GetElementsByTagName("infCte")[0];

                if (elementInfCte.GetElementsByTagName("ide").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <ide>, do grupo de tag <CTeSimp><infCte>, não foi localizada no XML.");
                }
                var elementIde = (XmlElement)elementInfCte.GetElementsByTagName("ide")[0];

                Configuracoes.CodigoUF = Convert.ToInt32(elementIde.GetElementsByTagName("cUF")[0].InnerText);
                Configuracoes.TipoAmbiente = (TipoAmbiente)Convert.ToInt32(elementIde.GetElementsByTagName("tpAmb")[0].InnerText);
                Configuracoes.Modelo = (ModeloDFe)Convert.ToInt32(elementIde.GetElementsByTagName("mod")[0].InnerText);
                Configuracoes.TipoEmissao = (TipoEmissao)Convert.ToInt32(elementIde.GetElementsByTagName("tpEmis")[0].InnerText);

                if (elementInfCte.GetAttribute("versao").Length > 0)
                {
                    Configuracoes.SchemaVersao = elementInfCte.GetAttribute("versao");
                }
                else
                {
                    throw new Exception("O atributo obrigatório \"versao\" da tag <infCte>, do grupo de tag <CTeSimp>, não foi localizado no XML.");
                }

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Efetuar um Ajustes no XML do CTe logo depois de assinado
        /// </summary>
        protected override void AjustarXMLAposAssinado()
        {
            MontarQrCode();
            base.AjustarXMLAposAssinado();
        }

#if INTEROP

        /// <summary>
        /// Adicionar o retorno da consulta situação do CTeSimp na lista dos retornos para elaboração do XML de Distribuição
        /// </summary>
        /// <param name="item">Resultado da consulta situação do CTe</param>
        public void AddRetConsSitCTes(RetConsSitCTe item) => (RetConsSitCTes ?? (RetConsSitCTes = new List<RetConsSitCTe>())).Add(item);

#endif

        /// <summary>
        /// Propriedade com o conteúdo retornado da consulta situação do CTe
        /// </summary>
        public List<RetConsSitCTe> RetConsSitCTes = new List<RetConsSitCTe>();

        /// <summary>
        /// Propriedade contendo o XML da CTe com o protocolo de autorização anexado
        /// </summary>
        public Dictionary<string, CteSimpProc> CteSimpProcResults
        {
            get
            {
                if (Result.ProtCTe != null)
                {
                    if (CteSimpProcs.ContainsKey(CTeSimp.InfCTe.Chave))
                    {
                        CteSimpProcs[CTeSimp.InfCTe.Chave].ProtCTe = Result.ProtCTe;
                    }
                    else
                    {
                        CteSimpProcs.Add(CTeSimp.InfCTe.Chave,
                            new CteSimpProc
                            {
                                Versao = CTeSimp.InfCTe.Versao,
                                CTeSimp = CTeSimp,
                                ProtCTe = Result.ProtCTe
                            });
                    }
                }
                else
                {
                    if (RetConsSitCTes.Count <= 0)
                    {
                        throw new Exception("Defina o conteúdo da Propriedade RetConsSitCte, sem a definição dela não é possível obter o conteúdo da CteSimpProcResults.");
                    }

                    ProtCTe protCTe = null;

                    #region Resultado do envio do CTeSimp através da consulta situação

                    foreach (var item in RetConsSitCTes)
                    {
                        if (item != null && item.ProtCTe != null)
                        {
                            if (item.ProtCTe.InfProt.ChCTe == CTeSimp.InfCTe.Chave)
                            {
                                switch (item.ProtCTe.InfProt.CStat)
                                {
                                    case 100: //CTe Autorizado
                                    case 110: //CTe Denegado - Não sei quando ocorre este, mas descobrir ele no manual então estou incluindo. 
                                    case 301: //CTe Denegado - Irregularidade fiscal do emitente
                                    case 302: //CTe Denegado - Irregularidade fiscal do remetente
                                    case 303: //CTe Denegado - Irregularidade fiscal do destinatário
                                    case 304: //CTe Denegado - Irregularidade fiscal do expedidor
                                    case 305: //CTe Denegado - Irregularidade fiscal do recebedor
                                    case 306: //CTe Denegado - Irregularidade fiscal do tomador
                                        protCTe = item.ProtCTe;
                                        break;
                                }
                            }
                        }
                    }

                    if (CteSimpProcs.ContainsKey(CTeSimp.InfCTe.Chave))
                    {
                        CteSimpProcs[CTeSimp.InfCTe.Chave].ProtCTe = protCTe;
                    }
                    else
                    {
                        CteSimpProcs.Add(CTeSimp.InfCTe.Chave,
                            new CteSimpProc
                            {
                                Versao = CTeSimp.InfCTe.Versao,
                                CTeSimp = CTeSimp,
                                ProtCTe = protCTe
                            });
                    }

                    #endregion
                }

                return CteSimpProcs;
            }
        }

#if INTEROP

        /// <summary>
        /// Recupera o XML de distribuição do CTeSimp no formato string
        /// </summary>
        /// <param name="chaveDFe">Chave do CTeSimp que é para retornar o XML de distribuição</param>
        /// <returns>XML de distribuição do CTeSimp</returns>
        public string GetCteSimpProcResults(string chaveDFe)
        {
            var retornar = "";
            if (CteSimpProcResults.Count > 0)
            {
                retornar = CteSimpProcResults[chaveDFe].GerarXML().OuterXml;
            }

            return retornar;
        }

#endif

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetCTeSimp Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetCTeSimp>(RetornoWSXML);
                }

                return new RetCTeSimp
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public AutorizacaoSimp() : base() => CteSimpProcs.Clear();

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="cteSimp">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public AutorizacaoSimp(CTeSimp cteSimp, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(cteSimp?.GerarXML() ?? throw new ArgumentNullException(nameof(cteSimp)), configuracao);
            CTeSimp = CTeSimp.LerXML<CTeSimp>(ConteudoXML);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public AutorizacaoSimp(string conteudoXML, Configuracao configuracao) : this()
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
            CTeSimp = CTeSimp.LerXML<CTeSimp>(ConteudoXML);
            CTeSimp.Signature = null;
            CTeSimp.InfCTeSupl = null;

            //Gerar o XML novamente com base no objeto
            ConteudoXML = CTeSimp.GerarXML();

            //Forçar assinar e criar QRCode novamente
            _ = ConteudoXMLAssinado;

            //Atualizar o objeto novamente com o XML já assinado e com QRCode
            CTeSimp = CTeSimp.LerXML<CTeSimp>(ConteudoXML);

            #endregion
        }

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar() => base.Executar();

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="cteSimp">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar(CTeSimp cteSimp, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(cteSimp?.GerarXML() ?? throw new ArgumentNullException(nameof(cteSimp)), configuracao);
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
        /// <param name="cteSimp">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(CTeSimp cteSimp, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(cteSimp?.GerarXML() ?? throw new ArgumentNullException(nameof(cteSimp)), configuracao);
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
        public void GravarXmlDistribuicao(string pasta)
        {
            try
            {
                foreach (var item in CteSimpProcResults)
                {
                    if (item.Value.ProtCTe != null)
                    {
                        GravarXmlDistribuicao(pasta, item.Value.NomeArquivoDistribuicao, item.Value.GerarXML().OuterXml);
                    }
                    else
                    {
                        throw new Exception("Não foi localizado no retorno da consulta o protocolo da chave, abaixo, para a elaboração do arquivo de distribuição. Verifique se a chave ou recibo consultado estão de acordo com a informada na sequencia:\r\n\r\n" + Format.ChaveDFe(item.Key));
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
        public void GravarXmlDistribuicao(System.IO.Stream stream)
        {
            try
            {
                foreach (var item in CteSimpProcResults)
                {
                    if (item.Value.ProtCTe != null)
                    {
                        GravarXmlDistribuicao(stream, item.Value.GerarXML().OuterXml);
                    }
                    else
                    {
                        throw new Exception("Não foi localizado no retorno da consulta o protocolo da chave, abaixo, para a elaboração do arquivo de distribuição. Verifique se a chave ou recibo consultado estão de acordo com a informada na sequencia:\r\n\r\n" + Format.ChaveDFe(item.Key));
                    }
                }
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }
    }
}