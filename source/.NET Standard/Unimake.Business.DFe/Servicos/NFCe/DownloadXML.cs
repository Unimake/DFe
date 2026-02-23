#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
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

        #endregion Public Methods
    }
}