#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CTe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CTe
{
    /// <summary>
    /// Envio do XML de CTe para o WebService - Envio síncrono
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CTe.AutorizacaoSinc")]
    [ComVisible(true)]
#endif
    public class AutorizacaoSinc : ServicoBase, IInteropService<Xml.CTe.CTe>
    {
        /// <summary>
        /// Validar o XML do CTe e também o Modal específico
        /// </summary>
        /// <param name="xml">XML a ser validado</param>
        /// <param name="schemaArquivo">Nome do arquivo de schema's para ser utilizado na validação</param>
        /// <param name="targetNS">Namespace a ser utilizado na validação</param>
        private void ValidarXMLCTe(XmlDocument xml, string schemaArquivo, string targetNS)
        {
            var validar = new ValidarSchema();
            validar.Validar(xml, Configuracoes.TipoDFe.ToString() + "." + schemaArquivo, targetNS);

            if (!validar.Success)
            {
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }

        private Xml.CTe.CTe _CTe;
        private readonly Dictionary<string, CteProc> CteProcs = new Dictionary<string, CteProc>();

        /// <summary>
        /// Objeto do XML do CTe
        /// </summary>
        public Xml.CTe.CTe CTe
        {
            get => _CTe ?? (_CTe = new Xml.CTe.CTe().LerXML<Xml.CTe.CTe>(ConteudoXML));
            protected set => _CTe = value;
        }

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.CTeAutorizacaoSinc;

                if (ConteudoXML.GetElementsByTagName("CTe").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <CTe> não foi localizada no XML.");
                }
                var elementCTe = (XmlElement)ConteudoXML.GetElementsByTagName("CTe")[0];

                if (elementCTe.GetElementsByTagName("infCte").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <infCte>, do grupo de tag <CTe>, não foi localizada no XML.");
                }
                var elementInfCte = (XmlElement)elementCTe.GetElementsByTagName("infCte")[0];

                if (elementInfCte.GetElementsByTagName("ide").Count <= 0)
                {
                    throw new Exception("A tag obrigatória <ide>, do grupo de tag <CTe><infCte>, não foi localizada no XML.");
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
                    throw new Exception("O atributo obrigatório \"versao\" da tag <infCte>, do grupo de tag <CTe>, não foi localizado no XML.");
                }

                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Efetuar um Ajustse no XML da NFCe logo depois de assinado
        /// </summary>
        protected override void AjustarXMLAposAssinado()
        {
            QrCodeXmlHelper.MontarQrCodeCTe(ConteudoXML, Configuracoes);
            base.AjustarXMLAposAssinado();
        }

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            if (Configuracoes.SchemasEspecificos.Count > 0)
            {
                var schemaArquivo = Configuracoes.SchemasEspecificos["1"].SchemaArquivo; //De qualquer modal o xml de validação da parte geral é o mesmo, então vou pegar do número 1, pq tanto faz.

                #region Validar o XML geral

                ValidarXMLCTe(ConteudoXML, schemaArquivo, Configuracoes.TargetNS);

                #endregion Validar o XML geral

                #region Validar a parte específica de modal do CTe

                foreach (XmlElement itemCTe in ConteudoXMLAssinado.GetElementsByTagName("CTe"))
                {
                    var modal = string.Empty;

                    foreach (XmlElement itemIde in itemCTe.GetElementsByTagName("ide"))
                    {
                        modal = itemIde.GetElementsByTagName("modal")[0].InnerText;
                    }

                    foreach (XmlElement itemInfModal in itemCTe.GetElementsByTagName("infModal"))
                    {
                        var xmlEspecifico = new XmlDocument();
                        xmlEspecifico.LoadXml(itemInfModal.InnerXml);
                        var schemaArquivoEspecifico = Configuracoes.SchemasEspecificos[modal.Substring(1, 1)].SchemaArquivoEspecifico;

                        ValidarXMLCTe(xmlEspecifico, schemaArquivoEspecifico, Configuracoes.TargetNS);
                    }
                }

                #endregion Validar a parte específica de cada evento
            }
        }

        /// <summary>
        /// Propriedade com o conteúdo retornado da consulta situação do CTe
        /// </summary>
        public List<RetConsSitCTe> RetConsSitCTe = new List<RetConsSitCTe>();

        /// <summary>
        /// Propriedade contendo o XML da CTe com o protocolo de autorização anexado - Envio Assíncrono
        /// </summary>
        public Dictionary<string, CteProc> CteProcResults
        {
            get
            {
                if (Result.ProtCTe != null)
                {
                    if (CteProcs.ContainsKey(CTe.InfCTe.Chave))
                    {
                        CteProcs[CTe.InfCTe.Chave].ProtCTe = Result.ProtCTe;
                    }
                    else
                    {
                        CteProcs.Add(CTe.InfCTe.Chave, new CteProc
                        {
                            Versao = CTe.InfCTe.Versao,
                            CTe = CTe,
                            ProtCTe = Result.ProtCTe
                        });
                    }
                }
                else
                {
                    if (RetConsSitCTe == null || RetConsSitCTe.Count <= 0)
                    {
                        throw new Exception("Defina o conteúdo da Propriedade RetConsSitCTe, sem a definição não é possível obter o conteúdo da CteProcResults.");
                    }

                    ProtCTe protCTe = null;

                    #region Resultado do envio do CT-e através da consulta situação

                    foreach (var item in RetConsSitCTe)
                    {
                        if (item != null && item.ProtCTe != null)
                        {
                            if (item.ProtCTe.InfProt.ChCTe == CTe.InfCTe.Chave)
                            {
                                switch (item.ProtCTe.InfProt.CStat)
                                {
                                    case 100: //CTe Autorizado
                                        protCTe = item.ProtCTe;
                                        break;
                                }
                            }
                        }
                    }

                    #endregion

                    if (CteProcs.ContainsKey(CTe.InfCTe.Chave))
                    {
                        CteProcs[CTe.InfCTe.Chave].ProtCTe = protCTe;
                    }
                    else
                    {
                        CteProcs.Add(CTe.InfCTe.Chave,
                            new CteProc
                            {
                                Versao = CTe.InfCTe.Versao,
                                CTe = CTe,
                                ProtCTe = protCTe
                            });
                    }
                }

                return CteProcs;
            }
        }

#if INTEROP

        /// <summary>
        /// Recupera o XML de distribuição do CTe no formato string
        /// </summary>
        /// <param name="chaveDFe">Chave do CTe que é para retornar o XML de distribuição</param>
        /// <returns>XML de distribuição do CTe</returns>
        public string GetCteProcResults(string chaveDFe)
        {
            var retornar = "";
            if (CteProcResults.Count > 0)
            {
                retornar = CteProcResults[chaveDFe].GerarXML().OuterXml;
            }

            return retornar;
        }

        /// <summary>
        /// Recupera o conteúdo de um CTe específico, assinado, existente no lote gerado de CTe´s
        /// </summary>
        /// <param name="index">Index do CTe existe no lote (Se no lote de CTe tem 3 CTe´s, pode ser 0, 1 ou 2, por exemplo.)</param>
        /// <returns>Retorna o conteúdo de um CTe específico, assinado, existente no lote gerado de CTe´s</returns>
        public string GetConteudoCTeAssinado(int index) => (ConteudoXMLAssinado != null ? ConteudoXMLAssinado.GetElementsByTagName("CTe")[index].OuterXml : "");

#endif

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetCTe Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetCTe>(RetornoWSXML);
                }

                return new RetCTe
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public AutorizacaoSinc() : base() => CteProcs.Clear();

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="cte">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public AutorizacaoSinc(Xml.CTe.CTe cte, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(cte?.GerarXML() ?? throw new ArgumentNullException(nameof(cte)), configuracao);

            CTe = CTe.LerXML<Xml.CTe.CTe>(ConteudoXML);
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
            CTe = CTe.LerXML<Xml.CTe.CTe>(ConteudoXML);
            CTe.Signature = null;
            CTe.InfCTeSupl = null;

            //Gerar o XML novamente com base no objeto
            ConteudoXML = CTe.GerarXML();

            //Forçar assinar e criar QRCode novamente
            _ = ConteudoXMLAssinado;

            //Atualizar o objeto novamente com o XML já assinado e com QRCode
            CTe = CTe.LerXML<Xml.CTe.CTe>(ConteudoXML);

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
        /// <param name="cte">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar(Xml.CTe.CTe cte, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(cte?.GerarXML() ?? throw new ArgumentNullException(nameof(cte)), configuracao);
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
        /// <param name="cte">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(Xml.CTe.CTe cte, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(cte?.GerarXML() ?? throw new ArgumentNullException(nameof(cte)), configuracao);
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
        public void AddRetConsSitCTe(RetConsSitCTe item) =>
            (RetConsSitCTe ?? (RetConsSitCTe = new List<RetConsSitCTe>())).Add(item);

#endif

        /// <summary>
        /// Gravar o XML de distribuição em uma pasta no HD
        /// </summary>
        /// <param name="pasta">Pasta onde deve ser gravado o XML</param>
        public void GravarXmlDistribuicao(string pasta)
        {
            try
            {
                foreach (var item in CteProcResults)
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
                Exceptions.ThrowHelper.Instance.Throw(ex);
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
                foreach (var item in CteProcResults)
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