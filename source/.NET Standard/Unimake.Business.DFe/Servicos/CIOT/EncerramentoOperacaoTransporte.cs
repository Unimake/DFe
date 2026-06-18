#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.CIOT;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Encerrar operação de transporte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.None)]
    [ComDefaultInterface(typeof(IEncerramentoOperacaoTransporteInterop))]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.EncerramentoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    public class EncerramentoOperacaoTransporte : ServicoBase<Xml.CIOT.EncerramentoOperacaoTransporte, RetEncerramentoOperacaoTransporte>, IInteropService<Xml.CIOT.EncerramentoOperacaoTransporte>
#if INTEROP
        , IEncerramentoOperacaoTransporteInterop
#endif
    {
        #region Private Fields

        private readonly System.Collections.Generic.Dictionary<string, EncerramentoOperacaoTransporteProc> EncerramentoOperacaoTransporteProcs = new System.Collections.Generic.Dictionary<string, EncerramentoOperacaoTransporteProc>();

        #endregion Private Fields

        #region Public Properties

        /// <summary>
        /// Propriedade contendo o XML do encerramento de operação de transporte com o retorno da API anexado para geração do arquivo de distribuição.
        /// A chave do dicionário é o código de identificação da operação retornado no campo <c>CodigoIdentificacaoOperacao</c>.
        /// </summary>
        public System.Collections.Generic.Dictionary<string, EncerramentoOperacaoTransporteProc> EncerramentoOperacaoTransporteProcResults
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

                if (EncerramentoOperacaoTransporteProcs.ContainsKey(codigoIdentificacaoOperacao))
                {
                    EncerramentoOperacaoTransporteProcs[codigoIdentificacaoOperacao].RetEncerramentoOperacaoTransporte = Result;
                }
                else
                {
                    EncerramentoOperacaoTransporteProcs.Add(codigoIdentificacaoOperacao, new EncerramentoOperacaoTransporteProc
                    {
                        Versao = Configuracoes.SchemaVersao,
                        EncerramentoOperacaoTransporte = Envio,
                        RetEncerramentoOperacaoTransporte = Result
                    });
                }

                return EncerramentoOperacaoTransporteProcs;
            }
        }

        /// <summary>
        /// Recupera o conteúdo processado do encerramento de operação de transporte, combinando o XML de envio com o XML de retorno para geração do arquivo de distribuição.
        /// </summary>
        public EncerramentoOperacaoTransporteProc EncerramentoOperacaoTransporteProcResult
        {
            get
            {
                var codigoIdentificacaoOperacao = Result?.CodigoIdentificacaoOperacao;

                if (string.IsNullOrWhiteSpace(codigoIdentificacaoOperacao))
                {
                    _ = EncerramentoOperacaoTransporteProcResults;
                    codigoIdentificacaoOperacao = Result.CodigoIdentificacaoOperacao;
                }

                return EncerramentoOperacaoTransporteProcResults[codigoIdentificacaoOperacao];
            }
        }

        #endregion Public Properties

        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTEncerramentoOperacaoTransporte;

        /// <summary>
        /// Construtor
        /// </summary>
        public EncerramentoOperacaoTransporte() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public EncerramentoOperacaoTransporte(Xml.CIOT.EncerramentoOperacaoTransporte xml, Configuracao configuracao) : this()
        {
            InicializarServico(xml, configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public EncerramentoOperacaoTransporte(string conteudoXML, Configuracao configuracao) : this()
        {
            InicializarServico(conteudoXML, configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço
        /// </summary>
        [ComVisible(true)]
        public void Executar(Xml.CIOT.EncerramentoOperacaoTransporte xml, Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }

        /// <summary>
        /// Recupera o XML processado do encerramento de operação de transporte no formato string para consumo por linguagens que acessam a biblioteca via COM/Interop.
        /// </summary>
        /// <returns>XML de distribuição no formato EncerramentoOperacaoTransporteProc.</returns>
        [ComVisible(true)]
        public string GetEncerramentoOperacaoTransporteProcResult() => EncerramentoOperacaoTransporteProcResult.GerarXML().OuterXml;

        /// <summary>
        /// Recupera o XML de distribuição do encerramento de operação de transporte no formato string a partir do código de identificação da operação, permitindo acesso por linguagens que utilizam COM/Interop.
        /// </summary>
        /// <param name="codigoIdentificacaoOperacao">Código de identificação da operação de transporte para retornar o XML de distribuição correspondente.</param>
        /// <returns>XML de distribuição do encerramento no formato EncerramentoOperacaoTransporteProc.</returns>
        [ComVisible(true)]
        public string GetEncerramentoOperacaoTransporteProcResults(string codigoIdentificacaoOperacao)
        {
            var retornar = "";

            if (EncerramentoOperacaoTransporteProcResults.Count > 0)
            {
                retornar = EncerramentoOperacaoTransporteProcResults[codigoIdentificacaoOperacao].GerarXML().OuterXml;
            }

            return retornar;
        }
#endif

        #region Public Methods

        /// <summary>
        /// Grava o XML de distribuição do encerramento de operação de transporte em uma pasta do disco, gerando automaticamente o nome do arquivo com base no conteúdo retornado pela API.
        /// </summary>
        /// <param name="pasta">Pasta onde o XML de distribuição será gravado.</param>
        public void GravarXmlDistribuicao(string pasta)
        {
            try
            {
                var encerramentoOperacaoTransporteProc = EncerramentoOperacaoTransporteProcResult;
                GravarXmlDistribuicao(pasta, encerramentoOperacaoTransporteProc.NomeArquivoDistribuicao, encerramentoOperacaoTransporteProc.GerarXML().OuterXml);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Grava o XML de distribuição do encerramento de operação de transporte em um stream informado pelo consumidor, permitindo manipulação em memória ou persistência customizada.
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

                var value = EncerramentoOperacaoTransporteProcResult.GerarXML().OuterXml;
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
