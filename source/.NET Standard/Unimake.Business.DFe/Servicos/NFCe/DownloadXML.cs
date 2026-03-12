#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NFCe
{
    /// <summary>
    /// Enviar o XML de download de NFCe para o webservice (Disponível apenas para SP)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFCe.DownloadXML")]
    [ComVisible(true)]
#endif
    public class DownloadXML : NFe.ServicoBase
    {
        #region Public Properties

        /// <summary>
        /// Conteúdo retornado pelo webservice depois do envio do XML
        /// </summary>
        public RetNfceDownloadXML Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return new RetNfceDownloadXML().LoadFromXML(RetornoWSString);
                }

                return new RetNfceDownloadXML
                {
                    CStat = "0",
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public DownloadXML() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="nfceDownloadXML">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public DownloadXML(NFCeDownloadXML nfceDownloadXML, Configuracao configuracao) : base()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Configuracoes = configuracao;
            ConteudoXML = nfceDownloadXML.GerarXML();
            Configuracoes.SchemaArquivo = nfceDownloadXML.GetType().Name + "-" + nfceDownloadXML.Versao.Replace(".", "") + ".xsd";
            Configuracoes.SchemaVersao = nfceDownloadXML.Versao;
            Configuracoes.Servico = Servico.NFCeDownloadXML;
            DefinirConfiguracao();
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public DownloadXML(string conteudoXML, Configuracao configuracao) : base()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var nfceDownloadXML = new NFCeDownloadXML().LoadFromXML(conteudoXML);
            Configuracoes = configuracao;
            ConteudoXML = nfceDownloadXML.GerarXML();
            Configuracoes.SchemaArquivo = nfceDownloadXML.GetType().Name + "-" + nfceDownloadXML.Versao.Replace(".", "") + ".xsd";
            Configuracoes.SchemaVersao = nfceDownloadXML.Versao;
            Configuracoes.Servico = Servico.NFCeDownloadXML;
            DefinirConfiguracao();
        }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executar o serviço
        /// </summary>
        /// <param name="nfceDownloadXML">Objeto do XML de download de NFCe</param>
        /// <param name="configuracao">Configuração para conexão e envio do XML</param>
        [ComVisible(true)]
        public void Executar(NFCeDownloadXML nfceDownloadXML, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Configuracoes = configuracao;
                ConteudoXML = nfceDownloadXML.GerarXML();
                Configuracoes.SchemaArquivo = nfceDownloadXML.GetType().Name + "-" + nfceDownloadXML.Versao.Replace(".", "") + ".xsd";
                Configuracoes.Servico = Servico.NFCeDownloadXML;

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
        /// Grava o XML de distribuição em uma pasta no HD
        /// </summary>
        /// <param name="pasta">Pasta onde deve ser gravado o XML</param>
        public void GravarXmlDistribuicao(string pasta)
        {
            var nfceDownloadXML = new NFCeDownloadXML().LoadFromXML(ConteudoXML.OuterXml);
            GravarXmlDistribuicao(pasta, nfceDownloadXML.ChNFCe + "-nfceDownloadXML.xml", ConteudoXML.OuterXml);
        }

#endif

        /// <summary>
        /// Grava os XMLs de distribuição (NFCe e eventos) em uma pasta no HD
        /// </summary>
        /// <param name="folder">Pasta onde os XMLs serão gravados</param>
        public void GravarXMLProc(string folder)
        {
            try
            {
                if (Result.Proc == null)
                {
                    return;
                }

                if (Result.Proc.NfeProc != null && !string.IsNullOrEmpty(Result.Proc.NfeProc.NFeXML))
                {
                    var xmlNFCe = new XmlDocument();
                    xmlNFCe.LoadXml(Result.Proc.NfeProc.NFeXML);

                    var infNFe = xmlNFCe.GetElementsByTagName("infNFe");
                    if (infNFe.Count > 0)
                    {
                        var elementInfNFe = (XmlElement)infNFe[0];
                        var chave = elementInfNFe.GetAttribute("Id")?.Replace("NFe", "");

                        if (!string.IsNullOrEmpty(chave))
                        {
                            var nomeArquivo = chave + "-nfce.xml";
                            base.GravarXmlDistribuicao(folder, nomeArquivo, Result.Proc.NfeProc.NFeXML);
                        }
                    }
                }

                if (Result.Proc.ProcEventoNFe != null && Result.Proc.ProcEventoNFe.Count > 0)
                {
                    foreach (var procEvento in Result.Proc.ProcEventoNFe)
                    {
                        if (!string.IsNullOrEmpty(procEvento.EventoXML))
                        {
                            var xmlEvento = new XmlDocument();
                            xmlEvento.LoadXml(procEvento.EventoXML);

                            var infEvento = xmlEvento.GetElementsByTagName("infEvento");
                            if (infEvento.Count > 0)
                            {
                                var elementInfEvento = (XmlElement)infEvento[0];
                                var chNFe = XMLUtility.TagRead(elementInfEvento, "chNFe");
                                var tpEvento = XMLUtility.TagRead(elementInfEvento, "tpEvento");
                                var nSeqEvento = XMLUtility.TagRead(elementInfEvento, "nSeqEvento");

                                if (!string.IsNullOrEmpty(chNFe) && !string.IsNullOrEmpty(tpEvento) && !string.IsNullOrEmpty(nSeqEvento))
                                {
                                    var nomeArquivo = chNFe + "_" + tpEvento + "_" + nSeqEvento.PadLeft(2, '0') + "-evento.xml";
                                    base.GravarXmlDistribuicao(folder, nomeArquivo, procEvento.EventoXML);
                                }
                            }
                        }
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