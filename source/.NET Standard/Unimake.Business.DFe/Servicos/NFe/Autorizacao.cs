#if INTEROP
using System.Runtime.InteropServices;
using Unimake.Exceptions;
#endif
using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Servicos.NFe
{
    /// <summary>
    /// Enviar o XML de NFe para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFe.Autorizacao")]
    [ComVisible(true)]
#endif
    public class Autorizacao : ServicoBase, IInteropService<EnviNFe>
    {
        #region Private Fields

        private EnviNFe _enviNFe;
        private readonly Dictionary<string, NfeProc> NfeProcs = new Dictionary<string, NfeProc>();

        #endregion Private Fields

        #region Private Methods

        /// <summary>
        /// Mudar o conteúdo da tag xMotivo caso a nota tenha sido rejeitada por problemas nos itens/produtos da nota. Assim vamos retornar na xMotivo algumas informações a mais para facilitar o entendimento para o usuário.
        /// </summary>
        private void MudarConteudoTagRetornoXMotivo()
        {
            if (EnviNFe.IndSinc == SimNao.Sim)
            {
                try
                {
                    if (RetornoWSXML.GetElementsByTagName("xMotivo").Count > 0)
                    {
                        var xMotivo = RetornoWSXML.GetElementsByTagName("xMotivo")[0].InnerText;
                        if (!string.IsNullOrWhiteSpace(xMotivo))
                        {
                            var xMotivoLower = xMotivo.ToLower();
                            if (xMotivoLower.Contains("[nitem:"))
                            {
                                var nItemIndex = xMotivoLower.IndexOf("[nitem:") + 7;
                                var nItemString = xMotivo.Substring(nItemIndex).Split(']')[0];

                                if (int.TryParse(nItemString, out var nItem) && nItem > 0)
                                {
                                    if (EnviNFe.NFe.Count > 0 &&
                                        EnviNFe.NFe[0].InfNFe.Count > 0 &&
                                        (nItem - 1) < EnviNFe.NFe[0].InfNFe[0].Det.Count)
                                    {
                                        var det = EnviNFe.NFe[0].InfNFe[0].Det[nItem - 1];
                                        if (det?.Prod != null)
                                        {
                                            var cProd = det.Prod.CProd;
                                            var xProd = det.Prod.XProd;

                                            RetornoWSString = RetornoWSString.Replace(
                                                xMotivo,
                                                $"{xMotivo} [cProd:{cProd}] [xProd:{xProd}]"
                                            );

                                            RetornoWSXML.LoadXml(RetornoWSString); // Atualizar XML
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                catch { }
            }
        }

        #endregion

        #region Protected Properties

        /// <summary>
        /// Objeto do XML da NFe
        /// </summary>
        public EnviNFe EnviNFe
        {
            get => _enviNFe ?? (_enviNFe = new EnviNFe().LerXML<EnviNFe>(ConteudoXML));
            protected set => _enviNFe = value;
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
                Configuracoes.Servico = Servico.NFeAutorizacao;

                if (ConteudoXML.GetElementsByTagName("enviNFe").Count > 0)
                {
                    var tagEnviNFe = (XmlElement)ConteudoXML.GetElementsByTagName("enviNFe")[0];
                    if (tagEnviNFe.GetElementsByTagName("infNFe").Count > 0)
                    {
                        var tagInfNFe = (XmlElement)tagEnviNFe.GetElementsByTagName("infNFe")[0];

                        if (tagInfNFe.GetAttribute("versao").Length > 0)
                        {
                            Configuracoes.SchemaVersao = tagInfNFe.GetAttribute("versao");
                        }
                        else
                        {
                            throw new Exception("O atributo obrigatório \"versao\" da tag <infNFe>, do grupo de tag <enviNFe><NFe>, não foi localizado no XML.");
                        }

                        if (tagInfNFe.GetElementsByTagName("ide").Count > 0)
                        {
                            var tagIde = (XmlElement)tagInfNFe.GetElementsByTagName("ide")[0];

                            if (tagIde.GetElementsByTagName("cUF").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <cUF>, do grupo de tag <enviNFe><NFe><infNFe><ide>, não foi localizada no XML.");
                            }
                            else if (tagIde.GetElementsByTagName("mod").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <mod>, do grupo de tag <enviNFe><NFe><infNFe><ide>, não foi localizada no XML.");
                            }
                            else if (tagIde.GetElementsByTagName("tpEmis").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <tpEmis>, do grupo de tag <enviNFe><NFe><infNFe><ide>, não foi localizada no XML.");
                            }
                            else if (tagIde.GetElementsByTagName("tpAmb").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <tpAmb>, do grupo de tag <enviNFe><NFe><infNFe><ide>, não foi localizada no XML.");
                            }

                            Configuracoes.CodigoUF = Convert.ToInt32(tagIde.GetElementsByTagName("cUF")[0].InnerText);
                            Configuracoes.Modelo = (ModeloDFe)Convert.ToInt32(tagIde.GetElementsByTagName("mod")[0].InnerText);
                            Configuracoes.TipoEmissao = (TipoEmissao)Convert.ToInt32(tagIde.GetElementsByTagName("tpEmis")[0].InnerText);
                            Configuracoes.TipoAmbiente = (TipoAmbiente)Convert.ToInt32(tagIde.GetElementsByTagName("tpAmb")[0].InnerText);
                        }
                        else
                        {
                            throw new Exception("A tag obrigatória <ide>, do grupo de tag <enviNFe><infNFe>, não foi localizada no XML.");
                        }
                    }
                    else
                    {
                        throw new Exception("A tag obrigatória <infNFe>, do grupo de tag <enviNFe>, não foi localizada no XML.");
                    }
                }
                else
                {
                    throw new Exception("A tag obrigatória <enviNFe> não foi localizada no XML.");
                }

                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Propriedade com o conteúdo retornado da consulta recibo
        /// </summary>
        public RetConsReciNFe RetConsReciNFe { get; set; }

#if INTEROP

        /// <summary>
        /// Atribuir null para a propriedade RetConsReciNFe. (Em FOXPRO não conseguimos atribuir NULL diretamente na propriedade, dá erro de OLE)
        /// </summary>
        public void SetNullRetConsReciNFe() => RetConsReciNFe = null;

        /// <summary>
        /// Adicionar o retorno da consulta situação da NFe na lista dos retornos para elaboração do XML de distribuição
        /// </summary>
        /// <param name="item">Resultado da consulta situação do NFe</param>
        public void AddRetConsSitNFes(RetConsSitNFe item) => (RetConsSitNFes ?? (RetConsSitNFes = new List<RetConsSitNFe>())).Add(item);

#endif


        /// <summary>
        /// Lista com o conteúdo retornado das consultas situação do NFes enviadas
        /// </summary>
        public List<RetConsSitNFe> RetConsSitNFes = new List<RetConsSitNFe>();

        /// <summary>
        /// Propriedade contendo o XML da NFe com o protocolo de autorização anexado - Funciona para envio Assíncrono ou Síncrono
        /// </summary>
        public Dictionary<string, NfeProc> NfeProcResults
        {
            get
            {
                switch (EnviNFe.IndSinc)
                {
                    case SimNao.Nao: //Envio assíncrono
                        goto default;

                    case SimNao.Sim: //Envio síncrono
                        if (Result.ProtNFe != null)
                        {
                            var autorizado = false;
                            switch (Result.ProtNFe.InfProt.CStat)                                                                  
                            {
                                case 100: //Autorizado o uso da NF-e
                                case 110: //Uso Denegado
                                case 150: //Autorizado o uso da NF-e, autorização fora de prazo
                                case 205: //NF-e está denegada na base de dados da SEFAZ [nRec:999999999999999]
                                case 301: //Uso Denegado: Irregularidade fiscal do emitente
                                case 302: //Uso Denegado: Irregularidade fiscal do destinatário
                                case 303: //Uso Denegado: Destinatário não habilitado a operar na UF
                                    if (NfeProcs.ContainsKey(EnviNFe.NFe[0].InfNFe[0].Chave))
                                    {
                                        NfeProcs[EnviNFe.NFe[0].InfNFe[0].Chave].ProtNFe = Result.ProtNFe;
                                    }
                                    else
                                    {
                                        NfeProcs.Add(EnviNFe.NFe[0].InfNFe[0].Chave,
                                            new NfeProc
                                            {
                                                Versao = EnviNFe.Versao,
                                                NFe = EnviNFe.NFe[0],
                                                ProtNFe = Result.ProtNFe
                                            });
                                    }

                                    autorizado = true;

                                    break;

                                default: //Rejeitado
                                    if (RetConsSitNFes.Count <= 0)
                                    {
                                        goto case 100;
                                    }

                                    break;
                            }

                            if (!autorizado)
                            {
                                goto default;
                            }
                        }
                        else
                        {
                            goto default;
                        }
                        break;

                    default:
                        if (RetConsReciNFe == null && RetConsSitNFes.Count <= 0)
                        {
                            throw new Exception("Defina o conteúdo da Propriedade RetConsReciNFe ou RetConsSitNFe, sem a definição de uma delas não é possível obter o conteúdo da NFeProcResults.");
                        }

                        for (var i = 0; i < EnviNFe.NFe.Count; i++)
                        {
                            ProtNFe protNFe = null;

                            if (RetConsReciNFe != null && RetConsReciNFe.ProtNFe != null)
                            {
                                #region Resultado do envio do CT-e através da consulta recibo

                                if (RetConsReciNFe.CStat == 104) //Lote Processado
                                {
                                    foreach (var item in RetConsReciNFe.ProtNFe)
                                    {
                                        if (item.InfProt.ChNFe == EnviNFe.NFe[i].InfNFe[0].Chave)
                                        {
                                            switch (item.InfProt.CStat)
                                            {
                                                case 100: //Autorizado o uso da NF-e
                                                case 110: //Uso Denegado
                                                case 150: //Autorizado o uso da NF-e, autorização fora de prazo
                                                case 205: //NF-e está denegada na base de dados da SEFAZ [nRec:999999999999999]
                                                case 301: //Uso Denegado: Irregularidade fiscal do emitente
                                                case 302: //Uso Denegado: Irregularidade fiscal do destinatário
                                                case 303: //Uso Denegado: Destinatário não habilitado a operar na UF
                                                    protNFe = item;
                                                    break;
                                            }

                                            break;
                                        }
                                    }
                                }
                                #endregion
                            }
                            else if (RetConsSitNFes.Count > 0)
                            {
                                #region Resultado do envio do NF-e através da consulta situação

                                foreach (var item in RetConsSitNFes)
                                {
                                    if (item != null && item.ProtNFe != null)
                                    {
                                        if (item.ProtNFe.InfProt.ChNFe == EnviNFe.NFe[i].InfNFe[0].Chave)
                                        {
                                            switch (item.ProtNFe.InfProt.CStat)
                                            {
                                                case 100: //Autorizado o uso da NF-e
                                                case 110: //Uso Denegado
                                                case 150: //Autorizado o uso da NF-e, autorização fora de prazo
                                                case 205: //NF-e está denegada na base de dados da SEFAZ [nRec:999999999999999]
                                                case 301: //Uso Denegado: Irregularidade fiscal do emitente
                                                case 302: //Uso Denegado: Irregularidade fiscal do destinatário
                                                case 303: //Uso Denegado: Destinatário não habilitado a operar na UF
                                                    protNFe = item.ProtNFe;
                                                    break;
                                            }
                                        }
                                    }
                                }

                                #endregion
                            }

                            if (NfeProcs.ContainsKey(EnviNFe.NFe[i].InfNFe[0].Chave))
                            {
                                NfeProcs[EnviNFe.NFe[i].InfNFe[0].Chave].ProtNFe = protNFe;
                            }
                            else
                            {
                                //Se por algum motivo não tiver assinado, só vou forçar atualizar o ConteudoXML para ficar correto na hora de gerar o arquivo de distribuição. Pode estar sem assinar no caso do desenvolvedor estar forçando gerar o XML já autorizado a partir de uma consulta situação da NFe, caso tenha perdido na tentativa do primeiro envio.
                                if (EnviNFe.NFe[i].Signature == null)
                                {
                                    ConteudoXML = ConteudoXMLAssinado;
                                    AjustarXMLAposAssinado();
                                }

                                NfeProcs.Add(EnviNFe.NFe[i].InfNFe[0].Chave,
                                    new NfeProc
                                    {
                                        Versao = EnviNFe.Versao,
                                        NFe = EnviNFe.NFe[i],
                                        ProtNFe = protNFe
                                    });
                            }
                        }

                        break;
                }

                return NfeProcs;
            }
        }

#if INTEROP

        /// <summary>
        /// Recupera o XML de distribuição da NFe no formato string
        /// </summary>
        /// <param name="chaveDFe">Chave da NFe que é para retornar o XML de distribuição</param>
        /// <returns>XML de distribuição da NFe</returns>
        public string GetNFeProcResults(string chaveDFe)
        {
            var retornar = "";
            if (NfeProcResults.Count > 0)
            {
                retornar = NfeProcResults[chaveDFe].GerarXML().OuterXml;
            }

            return retornar;
        }

        /// <summary>
        /// Recupera o conteúdo de uma NFe específica, assinada, existente no lote gerado de NFe´s
        /// </summary>
        /// <param name="index">Index da NFe existe no lote (Se no lote de NFe tem 3 notas, pode ser 0, 1 ou 2, por exemplo.)</param>
        /// <returns>Retorna o conteúdo de uma NFe específica, assinada, existente no lote gerado de NFe´s</returns>
        public string GetConteudoNFeAssinada(int index) => (ConteudoXMLAssinado != null ? ConteudoXMLAssinado.GetElementsByTagName("NFe")[index].OuterXml : "");

#endif

        /// <summary>
        /// Propriedade contendo o XML da NFe com o protocolo de autorização anexado - Funciona somente para envio síncrono
        /// </summary>
        public NfeProc NfeProcResult
        {
            get
            {
                if (EnviNFe.IndSinc == SimNao.Sim) //Envio síncrono
                {
                    return new NfeProc
                    {
                        Versao = EnviNFe.Versao,
                        NFe = EnviNFe.NFe[0],
                        ProtNFe = Result.ProtNFe
                    };
                }
                else
                {
                    throw new Exception("Para envio assíncrono utilize a propriedade NFeProcResults para obter os XMLs com o protocolo anexado.");
                }
            }
        }

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetEnviNFe Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetEnviNFe>(RetornoWSXML);
                }

                return new RetEnviNFe
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
        public Autorizacao() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="enviNFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public Autorizacao(EnviNFe enviNFe, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(enviNFe?.GerarXML() ?? throw new ArgumentNullException(nameof(enviNFe)), configuracao);
            EnviNFe = EnviNFe.LerXML<EnviNFe>(ConteudoXML); //Atualizar o objeto com o conteúdo do XML
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public Autorizacao(string conteudoXML, Configuracao configuracao) : this()
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
            EnviNFe = EnviNFe.LerXML<EnviNFe>(ConteudoXML);
            foreach (var nfe in EnviNFe.NFe)
            {
                nfe.Signature = null;
                nfe.InfNFeSupl = null;
            }

            //Gerar o XML novamente com base no objeto
            ConteudoXML = EnviNFe.GerarXML();

            //Forçar assinar e criar QRCode novamente
            _ = ConteudoXMLAssinado;

            //Atualizar o objeto novamente com o XML já assinado e com QRCode
            EnviNFe = EnviNFe.LerXML<EnviNFe>(ConteudoXML);

            #endregion
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

            MudarConteudoTagRetornoXMotivo();
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="enviNFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        public void Executar(EnviNFe enviNFe, Configuracao configuracao)
        {
            try
            {
                Inicializar(enviNFe?.GerarXML() ?? throw new ArgumentNullException(nameof(enviNFe)), configuracao);

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
        /// <param name="enviNFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(EnviNFe enviNFe, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(enviNFe?.GerarXML() ?? throw new ArgumentNullException(nameof(enviNFe)), configuracao);
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
                foreach (var item in NfeProcResults)
                {
                    if (item.Value.ProtNFe != null)
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
#if INTEROP
        [ComVisible(false)]
#endif
        public void GravarXmlDistribuicao(System.IO.Stream stream)
        {
            try
            {
                foreach (var item in NfeProcResults)
                {
                    if (item.Value.ProtNFe != null)
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
                Exceptions.ThrowHelper.Instance.Throw(ex);
            }
        }

        #endregion Public Methods
    }
}