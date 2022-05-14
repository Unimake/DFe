#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.GNRE;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.GNRE
{
    /// <summary>
    /// Envia XML de consulta Processamento de Lote de Consulta GNRE para WebService
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.GNRE.ConsultaResultadoLoteConsulta")]
    [ComVisible(true)]
#endif
    public class ConsultaResultadoLoteConsulta : ServicoBase, IInteropService<TConsLoteConsGNRE>
    {
        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new TConsLoteConsGNRE();
            xml = xml.LerXML<TConsLoteConsGNRE>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.GNREConsultaResultadoLoteConsulta;
                Configuracoes.TipoAmbiente = xml.Ambiente;
                Configuracoes.CodigoUF = (int)UFBrasil.PR;
                Configuracoes.SchemaVersao = "2.00";

                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Contem o resultado da consulta do lote
        /// </summary>
        public TResultLoteGNRE Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<TResultLoteGNRE>(RetornoWSXML);
                }

                return new TResultLoteGNRE
                {
                    SituacaoProcess = new SituacaoProcess
                    {
                        Codigo = "0",
                        Descricao = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado do WebService."
                    }
                };
            }
        }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultaResultadoLoteConsulta() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="tConsLoteConsGNRE">Objeto contendo o XML da consulta do lote da GNRE</param>
        /// <param name="configuracao">Objeto contendo as configurações a serem utilizadas na consulta do lote da GNRE</param>
        public ConsultaResultadoLoteConsulta(TConsLoteConsGNRE tConsLoteConsGNRE, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(tConsLoteConsGNRE?.GerarXML() ?? throw new ArgumentNullException(nameof(tConsLoteConsGNRE)), configuracao);
        }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executa o envio da consulta do lote da GNRE
        /// </summary>
        /// <param name="tConsLoteConsGNRE">Objeto contendo o XML da consulta do lote da GNRE</param>
        /// <param name="configuracao">Objeto contendo as configurações a serem utilizadas na consulta do lote da GNRE</param>
        [ComVisible(true)]
        public void Executar(TConsLoteConsGNRE tConsLoteConsGNRE, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(tConsLoteConsGNRE?.GerarXML() ?? throw new ArgumentNullException(nameof(tConsLoteConsGNRE)), configuracao);
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
        /// Gravar o XML retornado na consulta da GNRE
        /// </summary>
        /// <param name="pasta">Pasta onde será gravado o XML retornado</param>
        /// <param name="nomeArquivo">Nome do arquivo que será gravado</param>
        public void GravarXmlRetorno(string pasta, string nomeArquivo) => GravarXmlDistribuicao(pasta, nomeArquivo);

        /// <summary>
        /// Grava o XML de Distribuição em uma pasta definida retornado pelo webservice
        /// </summary>
        /// <param name="pasta">Pasta onde é para ser gravado do XML</param>
        /// <param name="nomeArquivo">Nome para o arquivo XML</param>
        public void GravarXmlDistribuicao(string pasta, string nomeArquivo)
        {
            if (!string.IsNullOrWhiteSpace(RetornoWSString))
            {
                GravarXmlDistribuicao(pasta, nomeArquivo, RetornoWSString);
            }
        }

        #endregion Public Methods
    }
}