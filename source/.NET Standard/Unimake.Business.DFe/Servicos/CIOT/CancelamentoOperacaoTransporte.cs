#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Cancelar operação de transporte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.CancelamentoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    public class CancelamentoOperacaoTransporte : ServicoBase, IInteropService<Xml.CIOT.CancelamentoOperacaoTransporte>
    {
        #region Private Fields

        private Xml.CIOT.CancelamentoOperacaoTransporte envio;
        private readonly System.Collections.Generic.Dictionary<string, CancelamentoOperacaoTransporteProc> CancelamentoOperacaoTransporteProcs = new System.Collections.Generic.Dictionary<string, CancelamentoOperacaoTransporteProc>();

        #endregion Private Fields

        #region Public Properties

        /// <summary>
        /// Objeto do XML de envio
        /// </summary>
        public Xml.CIOT.CancelamentoOperacaoTransporte Envio => ObterEnvio(ref envio);

        /// <summary>
        /// Resultado do serviço
        /// </summary>
        public RetCancelamentoOperacaoTransporte Result => ObterResult<RetCancelamentoOperacaoTransporte>();

        /// <summary>
        /// Propriedade contendo o XML do cancelamento de operação de transporte com o retorno da API anexado para geração do arquivo de distribuição.
        /// A chave do dicionário é o código de identificação da operação retornado no campo <c>CodigoIdentificacaoOperacao</c>.
        /// </summary>
        [System.Runtime.InteropServices.ComVisible(false)]
        public System.Collections.Generic.Dictionary<string, CancelamentoOperacaoTransporteProc> CancelamentoOperacaoTransporteProcResults
        {
            get
            {
                if (Result?.Temp != null)
                {
                    throw new Exception($"Não é possível gerar o XML de distribuição porque o serviço retornou erro: {Result.Temp.Error} - {Result.Temp.Message}");
                }

                if (string.IsNullOrWhiteSpace(Result?.CodigoIdentificacaoOperacao))
                {
                    throw new Exception("Não foi localizado o CodigoIdentificacaoOperacao no retorno do serviço para elaboração do arquivo de distribuição.");
                }

                var codigoIdentificacaoOperacao = Result.CodigoIdentificacaoOperacao;

                if (CancelamentoOperacaoTransporteProcs.ContainsKey(codigoIdentificacaoOperacao))
                {
                    CancelamentoOperacaoTransporteProcs[codigoIdentificacaoOperacao].RetCancelamentoOperacaoTransporte = Result;
                }
                else
                {
                    CancelamentoOperacaoTransporteProcs.Add(codigoIdentificacaoOperacao, new CancelamentoOperacaoTransporteProc
                    {
                        Versao = Configuracoes.SchemaVersao,
                        CancelamentoOperacaoTransporte = Envio,
                        RetCancelamentoOperacaoTransporte = Result
                    });
                }

                return CancelamentoOperacaoTransporteProcs;
            }
        }

        /// <summary>
        /// Recupera o conteúdo processado do cancelamento de operação de transporte, combinando o XML de envio com o XML de retorno para geração do arquivo de distribuição.
        /// </summary>
        public CancelamentoOperacaoTransporteProc CancelamentoOperacaoTransporteProcResult
        {
            get
            {
                var codigoIdentificacaoOperacao = Result?.CodigoIdentificacaoOperacao;

                if (string.IsNullOrWhiteSpace(codigoIdentificacaoOperacao))
                {
                    _ = CancelamentoOperacaoTransporteProcResults;
                    codigoIdentificacaoOperacao = Result.CodigoIdentificacaoOperacao;
                }

                return CancelamentoOperacaoTransporteProcResults[codigoIdentificacaoOperacao];
            }
        }

        #endregion Public Properties

        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTCancelamentoOperacaoTransporte;

        /// <inheritdoc />
        protected override string NomeRootRetorno => nameof(RetCancelamentoOperacaoTransporte);

        /// <inheritdoc />
        protected override XMLBase XmlEnvio => Envio;

        /// <summary>
        /// Construtor
        /// </summary>
        public CancelamentoOperacaoTransporte() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public CancelamentoOperacaoTransporte(Xml.CIOT.CancelamentoOperacaoTransporte xml, Configuracao configuracao) : this()
        {
            InicializarServico(xml, configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public CancelamentoOperacaoTransporte(string conteudoXML, Configuracao configuracao) : this()
        {
            InicializarServico(conteudoXML, configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço
        /// </summary>
        [ComVisible(true)]
        public void Executar(Xml.CIOT.CancelamentoOperacaoTransporte xml, Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }

        /// <summary>
        /// Recupera o XML processado do cancelamento de operação de transporte no formato string para consumo por linguagens que acessam a biblioteca via COM/Interop.
        /// </summary>
        /// <returns>XML de distribuição no formato CancelamentoOperacaoTransporteProc.</returns>
        [ComVisible(true)]
        public string GetCancelamentoOperacaoTransporteProcResult() => CancelamentoOperacaoTransporteProcResult.GerarXML().OuterXml;

        /// <summary>
        /// Recupera o XML de distribuição do cancelamento de operação de transporte no formato string a partir do código de identificação da operação, permitindo acesso por linguagens que utilizam COM/Interop.
        /// </summary>
        /// <param name="codigoIdentificacaoOperacao">Código de identificação da operação de transporte para retornar o XML de distribuição correspondente.</param>
        /// <returns>XML de distribuição do cancelamento no formato CancelamentoOperacaoTransporteProc.</returns>
        [ComVisible(true)]
        public string GetCancelamentoOperacaoTransporteProcResults(string codigoIdentificacaoOperacao)
        {
            var retornar = "";

            if (CancelamentoOperacaoTransporteProcResults.Count > 0)
            {
                retornar = CancelamentoOperacaoTransporteProcResults[codigoIdentificacaoOperacao].GerarXML().OuterXml;
            }

            return retornar;
        }
#endif

        #region Public Methods

        /// <summary>
        /// Grava o XML de distribuição do cancelamento de operação de transporte em uma pasta do disco, gerando automaticamente o nome do arquivo com base no conteúdo retornado pela API.
        /// </summary>
        /// <param name="pasta">Pasta onde o XML de distribuição será gravado.</param>
        public void GravarXmlDistribuicao(string pasta)
        {
            try
            {
                var cancelamentoOperacaoTransporteProc = CancelamentoOperacaoTransporteProcResult;
                GravarXmlDistribuicao(pasta, cancelamentoOperacaoTransporteProc.NomeArquivoDistribuicao, cancelamentoOperacaoTransporteProc.GerarXML().OuterXml);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Grava o XML de distribuição do cancelamento de operação de transporte em um stream informado pelo consumidor, permitindo manipulação em memória ou persistência customizada.
        /// </summary>
        /// <param name="stream">Stream de destino que receberá o conteúdo do XML de distribuição.</param>
#if INTEROP
        [ComVisible(false)]
#endif
        public void GravarXmlDistribuicao(System.IO.Stream stream)
        {
            try
            {
                if (stream is null)
                {
                    throw new ArgumentNullException(nameof(stream));
                }

                var value = CancelamentoOperacaoTransporteProcResult.GerarXML().OuterXml;
                var byteData = System.Text.Encoding.UTF8.GetBytes(value);
                stream.Write(byteData, 0, byteData.Length);
                stream.Close();
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        #endregion Public Methods
    }
}
