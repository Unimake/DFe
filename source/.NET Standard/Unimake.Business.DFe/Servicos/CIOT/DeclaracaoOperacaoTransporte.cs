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
    /// Declarar operação de transporte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.None)]
    [ComDefaultInterface(typeof(IDeclaracaoOperacaoTransporteInterop))]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.DeclaracaoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    public class DeclaracaoOperacaoTransporte : ServicoBase<Xml.CIOT.DeclaracaoOperacaoTransporte, RetDeclaracaoOperacaoTransporte>, IInteropService<Xml.CIOT.DeclaracaoOperacaoTransporte>
#if INTEROP
        , IDeclaracaoOperacaoTransporteInterop
#endif
    {
        #region Private Fields

        private readonly System.Collections.Generic.Dictionary<string, DeclaracaoOperacaoTransporteProc> DeclaracaoOperacaoTransporteProcs = new System.Collections.Generic.Dictionary<string, DeclaracaoOperacaoTransporteProc>();

        #endregion Private Fields

        #region Public Properties

        /// <summary>
        /// Propriedade contendo o XML da declaração de operação de transporte com o retorno da API anexado para geração do arquivo de distribuição.
        /// A chave do dicionário é o identificador da operação retornado no campo <c>IdOperacaoTransporte</c>.
        /// </summary>
        public System.Collections.Generic.Dictionary<string, DeclaracaoOperacaoTransporteProc> DeclaracaoOperacaoTransporteProcResults
        {
            get
            {
                if (Result?.Temp != null)
                {
                    throw new Exception($"Não é possível gerar o XML de distribuição porque o serviço retornou erro: {Result.Temp.Error} - {Result.Temp.Message}");
                }

                if (string.IsNullOrWhiteSpace(Result?.IdOperacaoTransporte))
                {
                    throw new Exception("Não foi localizado o IdOperacaoTransporte no retorno do serviço para elaboração do arquivo de distribuição.");
                }

                var idOperacaoTransporte = Result.IdOperacaoTransporte;

                if (DeclaracaoOperacaoTransporteProcs.ContainsKey(idOperacaoTransporte))
                {
                    DeclaracaoOperacaoTransporteProcs[idOperacaoTransporte].RetDeclaracaoOperacaoTransporte = Result;
                }
                else
                {
                    DeclaracaoOperacaoTransporteProcs.Add(idOperacaoTransporte, new DeclaracaoOperacaoTransporteProc
                    {
                        Versao = Configuracoes.SchemaVersao,
                        DeclaracaoOperacaoTransporte = Envio,
                        RetDeclaracaoOperacaoTransporte = Result
                    });
                }

                return DeclaracaoOperacaoTransporteProcs;
            }
        }

        /// <summary>
        /// Recupera o conteúdo processado da declaração de operação de transporte, combinando o XML de envio com o XML de retorno para geração do arquivo de distribuição.
        /// </summary>
        public DeclaracaoOperacaoTransporteProc DeclaracaoOperacaoTransporteProcResult
        {
            get
            {
                var idOperacaoTransporte = Result?.IdOperacaoTransporte;

                if (string.IsNullOrWhiteSpace(idOperacaoTransporte))
                {
                    _ = DeclaracaoOperacaoTransporteProcResults;
                    idOperacaoTransporte = Result.IdOperacaoTransporte;
                }

                return DeclaracaoOperacaoTransporteProcResults[idOperacaoTransporte];
            }
        }

        #endregion Public Properties

        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTDeclaracaoOperacaoTransporte;

        /// <summary>
        /// Construtor
        /// </summary>
        public DeclaracaoOperacaoTransporte() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public DeclaracaoOperacaoTransporte(Xml.CIOT.DeclaracaoOperacaoTransporte xml, Configuracao configuracao) : this()
        {
            InicializarServico(xml, configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public DeclaracaoOperacaoTransporte(string conteudoXML, Configuracao configuracao) : this()
        {
            InicializarServico(conteudoXML, configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço de declaração de operação de transporte via COM, inicializando os dados de envio e configurações antes de transmitir a requisição para a API.
        /// </summary>
        [ComVisible(true)]
        public void Executar(Xml.CIOT.DeclaracaoOperacaoTransporte xml, Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }

        /// <summary>
        /// Recupera o XML processado da declaração de operação de transporte no formato string para consumo por linguagens que acessam a biblioteca via COM/Interop.
        /// </summary>
        /// <returns>XML de distribuição no formato DeclaracaoOperacaoTransporteProc.</returns>
        [ComVisible(true)]
        public string GetDeclaracaoOperacaoTransporteProcResult() => DeclaracaoOperacaoTransporteProcResult.GerarXML().OuterXml;

        /// <summary>
        /// Recupera o XML de distribuição da declaração de operação de transporte no formato string a partir do código de identificação da operação, permitindo acesso por linguagens que utilizam COM/Interop.
        /// </summary>
        /// <param name="idOperacaoTransporte">Identificador da operação de transporte para retornar o XML de distribuição correspondente.</param>
        /// <returns>XML de distribuição da declaração no formato DeclaracaoOperacaoTransporteProc.</returns>
        [ComVisible(true)]
        public string GetDeclaracaoOperacaoTransporteProcResults(string idOperacaoTransporte)
        {
            var retornar = "";

            if (DeclaracaoOperacaoTransporteProcResults.Count > 0)
            {
                retornar = DeclaracaoOperacaoTransporteProcResults[idOperacaoTransporte].GerarXML().OuterXml;
            }

            return retornar;
        }
#endif

        #region Public Methods

        /// <summary>
        /// Grava o XML de distribuição da declaração de operação de transporte em uma pasta do disco, gerando automaticamente o nome do arquivo com base no conteúdo retornado pela API.
        /// </summary>
        /// <param name="pasta">Pasta onde o XML de distribuição será gravado.</param>
        public void GravarXmlDistribuicao(string pasta)
        {
            try
            {
                var declaracaoOperacaoTransporteProc = DeclaracaoOperacaoTransporteProcResult;
                GravarXmlDistribuicao(pasta, declaracaoOperacaoTransporteProc.NomeArquivoDistribuicao, declaracaoOperacaoTransporteProc.GerarXML().OuterXml);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Grava o XML de distribuição da declaração de operação de transporte em um stream informado pelo consumidor, permitindo manipulação em memória ou persistência customizada.
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

                var value = DeclaracaoOperacaoTransporteProcResult.GerarXML().OuterXml;
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
