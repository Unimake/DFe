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
    /// Serviço de Listagem de Chaves de NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFCe.ConsultaChaves")]
    [ComVisible(true)]
#endif
    public class ConsultaChaves : NFe.ServicoBase
    {
        #region Public Properties

        /// <summary>
        /// Conteúdo retornado pelo webservice depois do envio do XML
        /// </summary>
        public RetNFCeListagemChaves Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return new RetNFCeListagemChaves().LoadFromXML(RetornoWSString);
                }

                return new RetNFCeListagemChaves
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
        public ConsultaChaves() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="nfceListagemChaves">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaChaves(NFCeListagemChaves nfceListagemChaves, Configuracao configuracao) : base()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Configuracoes = configuracao;
            ConteudoXML = nfceListagemChaves.GerarXML();
            Configuracoes.SchemaArquivo = nfceListagemChaves.GetType().Name + "-" + nfceListagemChaves.Versao.Replace(".", "") + ".xsd";
            Configuracoes.SchemaVersao = nfceListagemChaves.Versao;
            Configuracoes.Servico = Servico.NFCeConsultaChaves;
            DefinirConfiguracao();
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML</param>
        public ConsultaChaves(string conteudoXML, Configuracao configuracao) : base()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var nfceListagemChaves = new NFCeListagemChaves().LoadFromXML(conteudoXML);
            Configuracoes = configuracao;
            ConteudoXML = nfceListagemChaves.GerarXML();
            Configuracoes.SchemaArquivo = nfceListagemChaves.GetType().Name + "-" + nfceListagemChaves.Versao.Replace(".", "") + ".xsd";
            Configuracoes.SchemaVersao = nfceListagemChaves.Versao;
            Configuracoes.Servico = Servico.NFCeConsultaChaves;
            DefinirConfiguracao();
        }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executar o serviço
        /// </summary>
        /// <param name="nfceListagemChaves">Objeto do XML de listagem de NFCe</param>
        /// <param name="configuracao">Configuração para conexão e envio do XML</param>
        [ComVisible(true)]
        public void Executar(NFCeListagemChaves nfceListagemChaves, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Configuracoes = configuracao;
                ConteudoXML = nfceListagemChaves.GerarXML();
                Configuracoes.SchemaArquivo = nfceListagemChaves.GetType().Name + "-" + nfceListagemChaves.Versao.Replace(".", "") + ".xsd";
                Configuracoes.Servico = Servico.NFCeListagemChaves;

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
            var nfceListagemChaves = new NFCeListagemChaves().LoadFromXML(ConteudoXML.OuterXml);
            // Como é uma listagem por período, o nome do arquivo usa as datas para evitar sobreposição
            string nomeArquivo = $"nfceListagemChaves_{nfceListagemChaves.DataHoraInicial:yyyyMMddHHmm}.xml";
            GravarXmlDistribuicao(pasta, nomeArquivo, ConteudoXML.OuterXml);
        }

#endif

        #endregion Public Methods
    }
}