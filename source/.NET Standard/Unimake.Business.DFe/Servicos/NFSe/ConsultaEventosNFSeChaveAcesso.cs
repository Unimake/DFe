#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Net;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta;
using Unimake.Exceptions;
using NFSeNacional = Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.NFSe;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Serviço para consultar eventos da NFSe por chave de acesso (Padrão NACIONAL).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.ConsultaEventosNFSeChaveAcesso")]
    [ComVisible(true)]
#endif
    public class ConsultaEventosNFSeChaveAcesso : ServicoBase, IInteropService<Xml.NFSe.NACIONAL.Consulta.ConsultaEventosNFSeChaveAcesso>
    {
        #region Public Properties

        /// <summary>
        /// Resultado da consulta de eventos por chave de acesso.
        /// </summary>
#if INTEROP
        [ComVisible(true)]
#endif
        public RetDistribuicaoNFSe Result
        {
            get
            {
                if (string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return null;
                }

                try
                {
                    return XMLUtility.Deserializar<RetDistribuicaoNFSe>(RetornoWSXML);
                }
                catch
                {
                    // Quando a API retorna 404 (nota não encontrada), o corpo é HTML e não
                    // deserializa como RetDistribuicaoNFSe. Retornamos um objeto com status
                    // significativo para que o usuário da DLL não precise checar HttpStatusCode.
                    if (HttpStatusCode == HttpStatusCode.NotFound)
                    {
                        return new RetDistribuicaoNFSe
                        {
                            StatusProcessamento = "NENHUM_DOCUMENTO_LOCALIZADO"
                        };
                    }

                    return null;
                }
            }
        }

        /// <summary>
        /// Lista de itens do LoteDFe recebidos na resposta.
        /// </summary>
        public List<LoteDFe> NFSesRecebidas { get; private set; }

        /// <summary>
        /// Lista de NFSes desserializadas individualmente a partir do LoteDFe.
        /// </summary>
        public List<NFSeNacional> NFSesDesserializadas { get; private set; }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor padrão.
        /// </summary>
        public ConsultaEventosNFSeChaveAcesso() : base() { }

        /// <summary>
        /// Construtor.
        /// </summary>
        /// <param name="conteudoXML">XML contendo os parâmetros da consulta.</param>
        /// <param name="configuracao">Configurações para conexão e envio ao webservice.</param>
        public ConsultaEventosNFSeChaveAcesso(XmlDocument conteudoXML, Configuracao configuracao) : this()
            => Inicializar(conteudoXML, configuracao);

        /// <summary>
        /// Construtor.
        /// </summary>
        /// <param name="conteudoXML">String XML contendo os parâmetros da consulta.</param>
        /// <param name="configuracao">Configurações para conexão e envio ao webservice.</param>
        public ConsultaEventosNFSeChaveAcesso(string conteudoXML, Configuracao configuracao) : this()
        {
            var xmlDoc = new XmlDocument();
            xmlDoc.LoadXml(conteudoXML);
            Inicializar(xmlDoc, configuracao);
        }

        /// <summary>
        /// Construtor.
        /// </summary>
        /// <param name="consulta">Objeto XML com os parâmetros da consulta.</param>
        /// <param name="configuracao">Configurações para conexão e envio ao webservice.</param>
        public ConsultaEventosNFSeChaveAcesso(Xml.NFSe.NACIONAL.Consulta.ConsultaEventosNFSeChaveAcesso consulta, Configuracao configuracao) : this()
            => Inicializar(consulta?.GerarXML() ?? throw new ArgumentNullException(nameof(consulta)), configuracao);

        #endregion Public Constructors

        #region Protected Methods

        /// <summary>
        /// Define a configuração do serviço.
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.NFSeConsultarEventosNFSeChaveAcesso;
                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Methods

        /// <summary>
        /// Executa o serviço e, se houver documentos no retorno, popula as listas de NFSes.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            base.Executar();

            try
            {
                if (Result?.LoteDFe == null)
                {
                    return;
                }

                NFSesRecebidas = new List<LoteDFe>();
                NFSesDesserializadas = new List<NFSeNacional>();

                foreach (var item in Result.LoteDFe)
                {
                    try
                    {
                        NFSesRecebidas.Add(item);

                        var nfse = item.ArquivoXml;

                        if (nfse == null && !string.IsNullOrWhiteSpace(item.ConteudoXML))
                        {
                            nfse = XMLUtility.Deserializar<NFSeNacional>(item.ConteudoXML);
                            item.ArquivoXml = nfse;
                        }

                        if (nfse != null)
                        {
                            NFSesDesserializadas.Add(nfse);
                        }
                    }
                    catch
                    {
                    }
                }
            }
            catch
            {
            }
        }

        /// <summary>
        /// Grava os XMLs das NFSes recebidas no retorno em disco.
        /// </summary>
        /// <param name="pasta">Pasta onde os XMLs serão gravados.</param>
        public void GravarXMLNFSe(string pasta)
        {
            try
            {
                if (RetornoWSXML == null)
                {
                    return;
                }

                var lotesXml = RetornoWSXML.GetElementsByTagName("LoteDFe");
                if (lotesXml == null || lotesXml.Count == 0)
                {
                    return;
                }

                foreach (XmlElement loteXml in lotesXml)
                {
                    try
                    {
                        var chave = XMLUtility.TagRead(loteXml, "ChaveAcesso");
                        var arquivoXmlNodes = loteXml.GetElementsByTagName("ArquivoXml");

                        if (arquivoXmlNodes.Count == 0)
                        {
                            continue;
                        }

                        var arquivoXml = (XmlElement)arquivoXmlNodes[0];
                        var nfseElement = arquivoXml.FirstChild as XmlElement;

                        var xml = nfseElement?.OuterXml ?? arquivoXml.InnerText;
                        if (string.IsNullOrWhiteSpace(xml))
                        {
                            continue;
                        }

                        var nomeArquivo = $"{chave}-nfse.xml";
                        base.GravarXmlDistribuicao(pasta, nomeArquivo, xml);
                    }
                    catch
                    {
                    }
                }
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço via COM Interop.
        /// </summary>
        [ComVisible(true)]
        public void Executar(Xml.NFSe.NACIONAL.Consulta.ConsultaEventosNFSeChaveAcesso consulta, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(consulta?.GerarXML() ?? throw new ArgumentNullException(nameof(consulta)), configuracao);
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
        /// Adiciona uma NFSe à lista de desserializadas (para COM Interop).
        /// </summary>
        [ComVisible(true)]
        public void AddNFSeDesserializada(NFSeNacional nfse)
        {
            if (NFSesDesserializadas == null)
            {
                NFSesDesserializadas = new List<NFSeNacional>();
            }

            NFSesDesserializadas.Add(nfse);
        }

        /// <summary>
        /// Obtém uma NFSe desserializada pelo índice (para COM Interop).
        /// </summary>
        [ComVisible(true)]
        public NFSeNacional GetNFSeDesserializada(int index)
        {
            if (NFSesDesserializadas == null || index < 0 || index >= NFSesDesserializadas.Count)
            {
                return null;
            }

            return NFSesDesserializadas[index];
        }

        /// <summary>
        /// Retorna a quantidade de NFSes desserializadas (para COM Interop).
        /// </summary>
        [ComVisible(true)]
        public int GetNFSeDesserializadaCount() => NFSesDesserializadas?.Count ?? 0;
#endif

        #endregion Public Methods
    }
}
