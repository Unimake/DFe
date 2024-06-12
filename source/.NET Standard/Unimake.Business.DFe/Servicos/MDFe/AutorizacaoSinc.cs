#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.MDFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.MDFe
{
    /// <summary>
    /// Enviar o XML de MDFe para o webservice no modo síncrono
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.MDFe.AutorizacaoSinc")]
    [ComVisible(true)]
#endif
    public class AutorizacaoSinc : ServicoBase, IInteropService<Xml.MDFe.MDFe>
    {
        private Xml.MDFe.MDFe _MDFe;

        private readonly Dictionary<string, MdfeProc> MdfeProcs = new Dictionary<string, MdfeProc>();

        private void MontarQrCode()
        {
            if (ConteudoXML.GetElementsByTagName("MDFe").Count <= 0)
            {
                throw new Exception("A tag obrigatória <MDFe> não foi localizada no XML.");
            }
            var elementMDFe = (XmlElement)ConteudoXML.GetElementsByTagName("MDFe")[0];

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

                var urlQrCode = (Configuracoes.TipoAmbiente == TipoAmbiente.Homologacao ? Configuracoes.UrlQrCodeHomologacao : Configuracoes.UrlQrCodeProducao);
                var paramLinkQRCode = urlQrCode +
                    "?chMDFe=" + chave +
                    "&tpAmb=" + ((int)tpAmb).ToString();

                if (tpEmis == TipoEmissao.ContingenciaFSIA)
                {
                    paramLinkQRCode += "&sign=" + Converter.ToRSASHA1(Configuracoes.CertificadoDigital, chave);
                }

                var nodeMDFe = ConteudoXML.GetElementsByTagName("MDFe")[0];

                var namespaceURI = nodeMDFe.GetNamespaceOfPrefix("");
                XmlNode infMDFeSuplNode = ConteudoXML.CreateElement("infMDFeSupl", namespaceURI);
                XmlNode qrCodMDFeNode = ConteudoXML.CreateElement("qrCodMDFe", namespaceURI);
                qrCodMDFeNode.InnerText = paramLinkQRCode.Trim();
                infMDFeSuplNode.AppendChild(qrCodMDFeNode);
                nodeMDFe.AppendChild(infMDFeSuplNode);
                var nodeInfMDFe = (XmlNode)elementInfMDFe;
                nodeMDFe.InsertAfter(infMDFeSuplNode, nodeInfMDFe);
            }
        }

        /// <summary>
        /// Validar o XML do MDFe e também o Modal específico
        /// </summary>
        /// <param name="xml">XML a ser validado</param>
        /// <param name="schemaArquivo">Nome do arquivo de schemas para ser utilizado na validação</param>
        /// <param name="targetNS">Namespace a ser utilizado na validação</param>
        private void ValidarXMLMDFe(XmlDocument xml, string schemaArquivo, string targetNS)
        {
            var validar = new ValidarSchema();
            validar.Validar(xml, Configuracoes.TipoDFe.ToString() + "." + schemaArquivo, targetNS);

            if (!validar.Success)
            {
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }

        /// <summary>
        /// Efetuar um Ajustse no XML da NFCe logo depois de assinado
        /// </summary>
        protected override void AjustarXMLAposAssinado()
        {
            MontarQrCode();
            base.AjustarXMLAposAssinado();
        }

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.MDFeAutorizacaoSinc;

                if (ConteudoXML.GetElementsByTagName("MDFe").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <MDFe> não foi localizada no XML.");
                }
                var elementMDFe = (XmlElement)ConteudoXML.GetElementsByTagName("MDFe")[0];

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

                Configuracoes.CodigoUF = Convert.ToInt32(elementIde.GetElementsByTagName("cUF")[0].InnerText);
                Configuracoes.TipoAmbiente = (TipoAmbiente)Convert.ToInt32(elementIde.GetElementsByTagName("tpAmb")[0].InnerText);
                Configuracoes.Modelo = (ModeloDFe)Convert.ToInt32(elementIde.GetElementsByTagName("mod")[0].InnerText);
                Configuracoes.TipoEmissao = (TipoEmissao)Convert.ToInt32(elementIde.GetElementsByTagName("tpEmis")[0].InnerText);

                if (elementInfMDFe.GetAttribute("versao").Length > 0)
                {
                    Configuracoes.SchemaVersao = elementInfMDFe.GetAttribute("versao");
                }
                else
                {
                    throw new Exception("O atributo obrigatório \"versao\" da tag <infMDFe>, do grupo de tag <MDFe>, não foi localizado no XML.");
                }

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Propriedade com o conteúdo retornado da consulta situação do MDFe
        /// </summary>
        public List<RetConsSitMDFe> RetConsSitMDFe = new List<RetConsSitMDFe>();

        /// <summary>
        /// Objeto do XML do MDFe
        /// </summary>
        public Xml.MDFe.MDFe MDFe
        {
            get => _MDFe ?? (_MDFe = new Xml.MDFe.MDFe().LerXML<Xml.MDFe.MDFe>(ConteudoXML));
            protected set => _MDFe = value;
        }

        /// <summary>
        /// Propriedade contendo o XML da MDFe com o protocolo de autorização anexado
        /// </summary>
        public Dictionary<string, MdfeProc> MDFeProcResults
        {
            get
            {
                if (Result.ProtMDFe != null)
                {
                    if (MdfeProcs.ContainsKey(MDFe.InfMDFe.Chave))
                    {
                        MdfeProcs[MDFe.InfMDFe.Chave].ProtMDFe = Result.ProtMDFe;
                    }
                    else
                    {
                        MdfeProcs.Add(MDFe.InfMDFe.Chave, new MdfeProc
                        {
                            Versao = MDFe.InfMDFe.Versao,
                            MDFe = MDFe,
                            ProtMDFe = Result.ProtMDFe
                        });
                    }
                }
                else
                {
                    if (RetConsSitMDFe.Count <= 0)
                    {
                        throw new Exception("Defina o conteúdo da Propriedade RetConsSitMDFe, sem a definição dela não é possível obter o conteúdo da MDFeProcResults.");
                    }

                    ProtMDFe protMDFe = null;

                    #region Resultado do envio do MDFe através da consulta situação

                    foreach (var item in RetConsSitMDFe)
                    {
                        if (item != null && item.ProtMDFe != null)
                        {
                            if (item.ProtMDFe.InfProt.ChMDFe == MDFe.InfMDFe.Chave)
                            {
                                switch (item.ProtMDFe.InfProt.CStat)
                                {
                                    case 100: //MDFe Autorizado
                                        protMDFe = item.ProtMDFe;
                                        break;
                                }
                            }
                        }
                    }

                    if (MdfeProcs.ContainsKey(MDFe.InfMDFe.Chave))
                    {
                        MdfeProcs[MDFe.InfMDFe.Chave].ProtMDFe = protMDFe;
                    }
                    else
                    {
                        MdfeProcs.Add(MDFe.InfMDFe.Chave,
                            new MdfeProc
                            {
                                Versao = MDFe.InfMDFe.Versao,
                                MDFe = MDFe,
                                ProtMDFe = protMDFe
                            });
                    }

                    #endregion
                }

                return MdfeProcs;
            }
        }

#if INTEROP

        /// <summary>
        /// Recupera o XML de distribuição do MDFe no formato string
        /// </summary>
        /// <param name="chaveDFe">Chave do MDFe que é para retornar o XML de distribuição</param>
        /// <returns>XML de distribuição do MDFe</returns>
        public string GetMDFeProcResults(string chaveDFe)
        {
            var retornar = "";
            if (MDFeProcResults.Count > 0)
            {
                retornar = MDFeProcResults[chaveDFe].GerarXML().OuterXml;
            }

            return retornar;
        }

#endif

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetMDFe Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetMDFe>(RetornoWSXML);
                }

                return new RetMDFe
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public AutorizacaoSinc() : base() => MdfeProcs.Clear();

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="mdfe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public AutorizacaoSinc(Xml.MDFe.MDFe mdfe, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(mdfe?.GerarXML() ?? throw new ArgumentNullException(nameof(mdfe)), configuracao);

            MDFe = MDFe.LerXML<Xml.MDFe.MDFe>(ConteudoXML);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
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
            MDFe = MDFe.LerXML<Xml.MDFe.MDFe>(ConteudoXML);
            MDFe.Signature = null;
            MDFe.InfMDFeSupl = null;

            //Gerar o XML novamente com base no objeto
            ConteudoXML = MDFe.GerarXML();

            //Forçar assinar e criar QRCode novamente
            _ = ConteudoXMLAssinado;

            //Atualizar o objeto novamente com o XML já assinado e com QRCode
            MDFe = MDFe.LerXML<Xml.MDFe.MDFe>(ConteudoXML);

            #endregion
        }

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar() => base.Executar();

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            var schemaArquivo = string.Empty;
            var schemaArquivoEspecifico = string.Empty;

            if (Configuracoes.SchemasEspecificos.Count > 0)
            {
                if (ConteudoXML.GetElementsByTagName("MDFe").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <MDFe> não foi localizada no XML.");
                }
                var elementMDFe = (XmlElement)ConteudoXML.GetElementsByTagName("MDFe")[0];

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

                if (elementIde.GetElementsByTagName("modal").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <modal>, do grupo de tag <MDFe><infMDFe><ide>, não foi localizada no XML.");

                }

                var modal = Convert.ToInt32(elementIde.GetElementsByTagName("modal")[0].InnerText);

                schemaArquivo = Configuracoes.SchemasEspecificos[modal.ToString()].SchemaArquivo;
                schemaArquivoEspecifico = Configuracoes.SchemasEspecificos[modal.ToString()].SchemaArquivoEspecifico;
            }

            #region Validar o XML geral

            ValidarXMLMDFe(ConteudoXML, schemaArquivo, Configuracoes.TargetNS);

            #endregion Validar o XML geral

            #region Validar a parte específica de modal do MDFe

            var xmlEspecifico = new XmlDocument();
            foreach (XmlElement item in ConteudoXMLAssinado.GetElementsByTagName("infModal"))
            {
                xmlEspecifico.LoadXml(item.InnerXml);
            }
            ValidarXMLMDFe(xmlEspecifico, schemaArquivoEspecifico, Configuracoes.TargetNS);

            #endregion Validar a parte específica de modal do MDFe
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="mdfe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar(Xml.MDFe.MDFe mdfe, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(mdfe?.GerarXML() ?? throw new ArgumentNullException(nameof(mdfe)), configuracao);
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
        /// <param name="mdfe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(Xml.MDFe.MDFe mdfe, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(mdfe?.GerarXML() ?? throw new ArgumentNullException(nameof(mdfe)), configuracao);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Adiciona um retorno da consulta situação da MDF-e.
        /// Este método está disponível apenas para interop
        /// </summary>
        /// <param name="item">Item que será adicionado</param>
        [ComVisible(true)]
        public void AddRetConsSitMDFe(RetConsSitMDFe item) =>
            (RetConsSitMDFe ?? (RetConsSitMDFe = new List<RetConsSitMDFe>())).Add(item);


        /// <summary>
        /// Recupera o conteúdo o único MDFe, assinado, existente no lote gerado de MDFe´s
        /// </summary>
        /// <returns>Retorna o conteúdo do MDFe, assinado, existente no lote gerado de MDFe´s</returns>
        public string GetConteudoMDFeAssinado() => (ConteudoXMLAssinado != null ? ConteudoXMLAssinado.OuterXml : "");

#endif

        /// <summary>
        /// Gravar o XML de distribuição em uma pasta no HD
        /// </summary>
        /// <param name="pasta">Pasta onde deve ser gravado o XML</param>
        public void GravarXmlDistribuicao(string pasta)
        {
            try
            {
                foreach (var item in MDFeProcResults)
                {
                    if (item.Value.ProtMDFe != null)
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
#if INTEROP
        [ComVisible(false)]
#endif
        public void GravarXmlDistribuicao(Stream stream)
        {
            try
            {
                foreach (var item in MDFeProcResults)
                {
                    if (item.Value.ProtMDFe != null)
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