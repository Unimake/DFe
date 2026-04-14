#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL.Consulta;
using NFSeNacional = Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.NFSe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Serviço para consultar a distribuição de NFSe por NSU
    /// Inspirado em DistribuicaoDFe (NFe)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.ConsultarDistribuicaoNFSeNSU")]
    [ComVisible(true)]
#endif
    public class ConsultarDistribuicaoNFSeNSU : ServicoBase, IInteropService<DistribuicaoNFSe>
    {
        #region Protected Methods

        /// <summary>
        /// Define a configuração do serviço
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new DistribuicaoNFSe();
            xml = xml.LerXML<DistribuicaoNFSe>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.NFSeConsultarDistribuicaoNFSeNSU;

                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Conteúdo retornado pelo web-service após a consulta
        /// </summary>
        public RetDistribuicaoNFSe Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetDistribuicaoNFSe>(RetornoWSXML);
                }

                return new RetDistribuicaoNFSe
                {
                    StatusProcessamento = "ERRO",
                    TipoAmbiente = Configuracoes.TipoAmbiente.ToString()
                };
            }
        }

        /// <summary>
        /// Lista de NFSes recebidas desserializadas
        /// </summary>
        public List<LoteDFe> NFSesRecebidas { get; private set; }

        /// <summary>
        /// Lista de NFSes descompactadas e desserializadas individualmente
        /// </summary>
        public List<NFSeNacional> NFSesDesserializadas { get; private set; }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="distribuicaoNFSe">Objeto contendo os parâmetros de consulta</param>
        /// <param name="configuracao">Configurações para conexão e envio para o web-service</param>
        public ConsultarDistribuicaoNFSeNSU(DistribuicaoNFSe distribuicaoNFSe, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(distribuicaoNFSe?.GerarXML() ?? throw new ArgumentNullException(nameof(distribuicaoNFSe)), configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML contendo os parâmetros de consulta</param>
        /// <param name="configuracao">Configurações para conexão e envio para o web-service</param>
        public ConsultarDistribuicaoNFSeNSU(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            Inicializar(doc, configuracao);
        }

        /// <summary>
        /// Construtor com XmlDocument
        /// </summary>
        /// <param name="conteudoXML">XmlDocument contendo os parâmetros de consulta</param>
        /// <param name="configuracao">Configurações para conexão e envio para o web-service</param>
        public ConsultarDistribuicaoNFSeNSU(XmlDocument conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(conteudoXML, configuracao);
        }

        /// <summary>
        /// Construtor padrão
        /// </summary>
        public ConsultarDistribuicaoNFSeNSU() : base() { }

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Gravar os XMLs de NFSe recebidos do web-service em uma pasta local, utilizando a chave de acesso como nome do arquivo
        /// </summary>
        /// <param name="pasta">Caminho da pasta onde os arquivos serão gravados</param>
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
        /// Obtém uma NFSe desserializada da lista (para COM Interop)
        /// </summary>
        /// <param name="index">Índice da NFSe</param>
        /// <returns>NFSe no índice especificado</returns>
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
        /// Obtém a quantidade de NFSes desserializadas (para COM Interop)
        /// </summary>
        /// <returns>Quantidade de NFSes desserializadas</returns>
        [ComVisible(true)]
        public int GetNFSeDesserializadaCount()
        {
            return NFSesDesserializadas?.Count ?? 0;
        }

        /// <summary>
        /// Executa o serviço: valida e envia para o web-service
        /// </summary>
        /// <param name="distribuicaoNFSe">Objeto contendo os parâmetros de consulta</param>
        /// <param name="configuracao">Configurações a serem utilizadas</param>
#if INTEROP
        [ComVisible(false)]
#endif
        public void Executar(DistribuicaoNFSe distribuicaoNFSe, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(distribuicaoNFSe?.GerarXML() ?? throw new ArgumentNullException(nameof(distribuicaoNFSe)), configuracao);
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

#endif

        /// <summary>
        /// Executa o serviço: valida e envia para o web-service
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

        #endregion Public Methods
    }
}