#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFGas;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFGas
{
    /// <summary>
    /// Enviar o XML da NFGas para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFGas.AutorizacaoSinc")]
    [ComVisible(true)]
#endif
    public class AutorizacaoSinc : ServicoBase, IInteropService<Xml.NFGas.NFGas>
    {
        #region Private Fields

        private Xml.NFGas.NFGas _NFGas;
        private readonly Dictionary<string, NFGasProc> NFGasProcs = new Dictionary<string, NFGasProc>();

        #endregion Private Fields

        #region Protected Properties

        /// <summary>
        /// Objeto XML da NFGas
        /// </summary>
        public Xml.NFGas.NFGas NFGas
        {
            get => _NFGas ?? (_NFGas = new Xml.NFGas.NFGas().LerXML<Xml.NFGas.NFGas>(ConteudoXML));
            protected set => _NFGas = value;
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
                Configuracoes.Servico = Servico.NFGasAutorizacaoSinc;

                if (ConteudoXML.GetElementsByTagName("NFGas").Count > 0)
                {
                    var tagNFGas = (XmlElement)ConteudoXML.GetElementsByTagName("NFGas")[0];
                    if (tagNFGas.GetElementsByTagName("infNFGas").Count > 0)
                    {
                        var tagInfNFGas = (XmlElement)ConteudoXML.GetElementsByTagName("infNFGas")[0];

                        if (tagInfNFGas.GetAttribute("versao").Length > 0)
                        {
                            Configuracoes.SchemaVersao = tagInfNFGas.GetAttribute("versao");
                        }
                        else
                        {
                            throw new Exception("O atributo obrigatório \"versao\" da tag <infNFGas>, do grupo de tag <NFGas>, não foi localizado no XML.");
                        }

                        if (tagInfNFGas.GetElementsByTagName("ide").Count > 0)
                        {
                            var tagIde = (XmlElement)tagInfNFGas.GetElementsByTagName("ide")[0];

                            if (tagIde.GetElementsByTagName("cUF").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <cUF>, do grupo de tag <NFGas><infNFGas><ide>, não foi localizada no XML.");
                            }
                            else if (tagIde.GetElementsByTagName("mod").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <mod>, do grupo de tag <NFGas><infNFGas><ide>, não foi localizada no XML.");
                            }
                            else if (tagIde.GetElementsByTagName("tpEmis").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <tpEmis>, do grupo de tag <NFGas><infNFGas><ide>, não foi localizada no XML.");
                            }
                            else if (tagIde.GetElementsByTagName("tpAmb").Count <= 0)
                            {
                                throw new Exception("A tag obrigatória <tpAmb>, do grupo de tag <NFGas><infNFGas><ide>, não foi localizada no XML.");
                            }

                            Configuracoes.CodigoUF = Convert.ToInt32(tagIde.GetElementsByTagName("cUF")[0].InnerText);
                            Configuracoes.Modelo = (ModeloDFe)Convert.ToInt32(tagIde.GetElementsByTagName("mod")[0].InnerText);
                            Configuracoes.TipoEmissao = (TipoEmissao)Convert.ToInt32(tagIde.GetElementsByTagName("tpEmis")[0].InnerText);
                            Configuracoes.TipoAmbiente = (TipoAmbiente)Convert.ToInt32(tagIde.GetElementsByTagName("tpAmb")[0].InnerText);
                        }
                        else
                        {
                            throw new Exception("A tag obrigatória <ide>, do grupo de tag <NFGas><infNFGas>, não foi localizada no XML.");
                        }
                    }
                    else
                    {
                        throw new Exception("A tag obrigatória <infNFGas>, do grupo de tag <NFGas>, não foi localizada no XML.");
                    }
                }
                else
                {
                    throw new Exception("A tag obrigatória <NFGas> não foi localizada no XML.");
                }

                base.DefinirConfiguracao();
            }
        }
        /// <summary>
        /// Efetuar um Ajustse no XML da NFGas logo depois de assinado
        /// </summary>
        protected override void AjustarXMLAposAssinado()
        {
            QrCodeXmlHelper.MontarQrCodeNFGas(ConteudoXML, Configuracoes);
            base.AjustarXMLAposAssinado();
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Lista com o conteúdo retornado das consultas situação das NFGas enviadas
        /// </summary>
        public List<RetConsSitNFGas> RetConsSitNFGas = new List<RetConsSitNFGas>();

        /// <summary>
        /// Propriedade contendo o XML da NFGas com o protocolo de autorização anexado - Funciona para envio Assíncrono ou Síncrono
        /// </summary>
        public Dictionary<string, NFGasProc> NFGasProcResults
        {
            get
            {
                if (Result.ProtNFGas != null)
                {
                    if (NFGasProcs.ContainsKey(NFGas.InfNFGas.Chave))
                    {
                        NFGasProcs[NFGas.InfNFGas.Chave].ProtNFGas = Result.ProtNFGas;
                    }
                    else
                    {
                        NFGasProcs.Add(NFGas.InfNFGas.Chave, new NFGasProc
                        {
                            Versao = NFGas.InfNFGas.Versao,
                            NFGas = NFGas,
                            ProtNFGas = Result.ProtNFGas
                        });
                    }
                }
                else
                {
                    if (RetConsSitNFGas.Count <= 0)
                    {
                        throw new Exception("Defina o conteúdo da Propriedade RetConsSitNFGas, sem a definição dela não é possível obter o conteúdo da NFGasProcResults.");
                    }

                    ProtNFGas protNFGas = null;

                    #region Resultado do envio do NFGas através da consulta situação

                    foreach (var item in RetConsSitNFGas)
                    {
                        if (item != null && item.ProtNFGas != null)
                        {
                            if (item.ProtNFGas.InfProt.ChNFGas == NFGas.InfNFGas.Chave)
                            {
                                switch (item.ProtNFGas.InfProt.CStat)
                                {
                                    case 100: //NFGas autorizada
                                        protNFGas = item.ProtNFGas;
                                        break;
                                }
                            }
                        }
                    }

                    if (NFGasProcs.ContainsKey(NFGas.InfNFGas.Chave))
                    {
                        NFGasProcs[NFGas.InfNFGas.Chave].ProtNFGas = protNFGas;
                    }

                    else
                    {
                        NFGasProcs.Add(NFGas.InfNFGas.Chave, new NFGasProc
                        {
                            Versao = NFGas.InfNFGas.Versao,
                            NFGas = NFGas,
                            ProtNFGas = protNFGas
                        });
                    }

                    #endregion Resultado do envio do NFGas através da consulta situação
                }

                return NFGasProcs;
            }
        }

#if INTEROP

        /// <summary>
        /// Recupera o XML de distribuição do NFGas no formato string
        /// </summary>
        /// <param name="chaveDFe">Chave do NFGas que é para retornar o XML de distribuição</param>
        /// <returns>XML de distribuição do NFGas</returns>
        public string GetNFGasProcResults(string chaveDFe)
        {
            var retornar = "";
            if (NFGasProcResults.Count > 0)
            {
                retornar = NFGasProcResults[chaveDFe].GerarXML().OuterXml;
            }

            return retornar;
        }

#endif

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetNFGas Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetNFGas>(RetornoWSString);
                }

                return new RetNFGas
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
        public AutorizacaoSinc() : base() => NFGasProcs.Clear();

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="NFGas">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public AutorizacaoSinc(Xml.NFGas.NFGas NFGas, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(NFGas?.GerarXML() ?? throw new ArgumentNullException(nameof(NFGas)), configuracao);

            NFGas = NFGas.LerXML<Xml.NFGas.NFGas>(ConteudoXML);
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
            NFGas = NFGas.LerXML<Xml.NFGas.NFGas>(ConteudoXML);
            NFGas.Signature = null;
            NFGas.InfNFGasSupl = null;

            //Gerar o XML novamente com base no objeto
            ConteudoXML = NFGas.GerarXML();

            //Forçar assinar e criar o QRCode novamente
            _ = ConteudoXMLAssinado;

            //Atualizar o objeto novamente com o XML já assinado e com QRCode
            NFGas = NFGas.LerXML<Xml.NFGas.NFGas>(ConteudoXML);

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
        /// <param name="NFGas">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        public void Executar(Xml.NFGas.NFGas NFGas, Configuracao configuracao)
        {
            try
            {
                Inicializar(NFGas?.GerarXML() ?? throw new ArgumentNullException(nameof(NFGas)), configuracao);

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
        /// <param name="NFGas">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(Xml.NFGas.NFGas NFGas, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(NFGas?.GerarXML() ?? throw new ArgumentNullException(nameof(NFGas)), configuracao);
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
                foreach (var item in NFGasProcResults)
                {
                    if (item.Value.ProtNFGas != null)
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
                foreach (var item in NFGasProcResults)
                {
                    if (item.Value.ProtNFGas != null)
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


