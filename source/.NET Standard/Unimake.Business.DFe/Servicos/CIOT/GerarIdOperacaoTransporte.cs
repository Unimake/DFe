#if INTEROP
using System.Runtime.InteropServices;
#endif
using Newtonsoft.Json;
using System;
using System.Text.RegularExpressions;
using System.Xml;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.CIOT;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Gerar identificador da operação de transporte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.GerarIdOperacaoTransporte")]
    [ComVisible(true)]
#endif
    public class GerarIdOperacaoTransporte : ServicoBase<Xml.CIOT.GerarIdOperacaoTransporte, RetGerarIdOperacaoTransporte>, IInteropService<Xml.CIOT.GerarIdOperacaoTransporte>
    {
        #region Private Fields

        private readonly System.Collections.Generic.Dictionary<string, GerarIdOperacaoTransporteProc> GerarIdOperacaoTransporteProcs = new System.Collections.Generic.Dictionary<string, GerarIdOperacaoTransporteProc>();

        #endregion Private Fields

        #region Public Properties

        /// <summary>
        /// Propriedade contendo o XML da geração do identificador da operação de transporte com o retorno da API anexado para geração do arquivo de distribuição.
        /// A chave do dicionário é o identificador da operação retornado no campo <c>IdOperacaoTransporte</c>.
        /// </summary>
        public System.Collections.Generic.Dictionary<string, GerarIdOperacaoTransporteProc> GerarIdOperacaoTransporteProcResults
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

                if (GerarIdOperacaoTransporteProcs.ContainsKey(idOperacaoTransporte))
                {
                    GerarIdOperacaoTransporteProcs[idOperacaoTransporte].RetGerarIdOperacaoTransporte = Result;
                }
                else
                {
                    GerarIdOperacaoTransporteProcs.Add(idOperacaoTransporte, new GerarIdOperacaoTransporteProc
                    {
                        Versao = Configuracoes.SchemaVersao,
                        GerarIdOperacaoTransporte = Envio,
                        RetGerarIdOperacaoTransporte = Result
                    });
                }

                return GerarIdOperacaoTransporteProcs;
            }
        }

        /// <summary>
        /// Recupera o conteúdo processado da geração do identificador da operação de transporte, combinando o XML de envio com o XML de retorno para geração do arquivo de distribuição.
        /// </summary>
        public GerarIdOperacaoTransporteProc GerarIdOperacaoTransporteProcResult
        {
            get
            {
                var idOperacaoTransporte = Result?.IdOperacaoTransporte;

                if (string.IsNullOrWhiteSpace(idOperacaoTransporte))
                {
                    _ = GerarIdOperacaoTransporteProcResults;
                    idOperacaoTransporte = Result.IdOperacaoTransporte;
                }

                return GerarIdOperacaoTransporteProcResults[idOperacaoTransporte];
            }
        }

        #endregion Public Properties

        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTGerarIdOperacaoTransporte;

        /// <summary>
        /// Construtor
        /// </summary>
        public GerarIdOperacaoTransporte() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public GerarIdOperacaoTransporte(Xml.CIOT.GerarIdOperacaoTransporte xml, Configuracao configuracao) : this()
        {
            InicializarServico(NormalizarEnvio(xml), configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public GerarIdOperacaoTransporte(string conteudoXML, Configuracao configuracao) : this()
        {
            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            InicializarServico(NormalizarEnvio(new Xml.CIOT.GerarIdOperacaoTransporte().LerXML<Xml.CIOT.GerarIdOperacaoTransporte>(doc)), configuracao);
        }

        /// <summary>
        /// Processar retorno bruto recebido da API ANTT
        /// </summary>
        /// <param name="retorno">Conteúdo JSON ou texto puro recebido da API.</param>
        /// <returns>Retorno XML tipado.</returns>
        public RetGerarIdOperacaoTransporte ProcessarRetornoANTT(string retorno)
        {
            if (string.IsNullOrWhiteSpace(retorno))
            {
                throw new InvalidOperationException("A API ANTT retornou conteúdo vazio ao gerar o IdOperacaoTransporte.");
            }

            var doc = new XmlDocument();
            var conteudo = retorno.Trim();

            if (Regex.IsMatch(conteudo, @"^[0-9]{12}$"))
            {
                doc.LoadXml("<temp><IdOperacaoTransporte>" + conteudo + "</IdOperacaoTransporte></temp>");
            }
            else
            {
                try
                {
                    doc = JsonConvert.DeserializeXmlNode(conteudo, "temp");
                }
                catch (JsonException ex)
                {
                    throw new InvalidOperationException("A API ANTT retornou conteúdo inválido ao gerar o IdOperacaoTransporte.", ex);
                }
            }

            if (doc?.DocumentElement == null)
            {
                throw new InvalidOperationException("A API ANTT retornou conteúdo inválido ao gerar o IdOperacaoTransporte.");
            }

            RetornoWSXML = doc;
            return Result;
        }

        /// <inheritdoc />
        protected override XmlDocument CriarXMLRetornoTipado()
        {
            var retorno = RetornoWSXML;
            var root = retorno?.DocumentElement;

            if (root == null)
            {
                throw new InvalidOperationException("A API ANTT retornou conteúdo vazio ao gerar o IdOperacaoTransporte.");
            }

            if (root.LocalName == nameof(RetGerarIdOperacaoTransporte))
            {
                return retorno;
            }

            if (root.LocalName == "temp" && root["error"] != null)
            {
                var retornoErro = base.CriarXMLRetornoTipado();
                return new RetGerarIdOperacaoTransporte().LerXML<RetGerarIdOperacaoTransporte>(retornoErro).GerarXML();
            }

            if (root.LocalName == "temp" && ObterValor(root, "Sucesso") != null)
            {
                return CriarRetornoANTT(root).GerarXML();
            }

            var idOperacaoTransporte = ObterValor(root, "IdOperacaoTransporte") ?? ObterValor(root, "idOperacaoTransporte");
            if (Regex.IsMatch(idOperacaoTransporte ?? "", @"^[0-9]{12}$"))
            {
                return CriarRetornoSucesso(idOperacaoTransporte).GerarXML();
            }

            var mensagem = ObterValor(root, "Message") ?? ObterValor(root, "message");
            if (!string.IsNullOrWhiteSpace(mensagem))
            {
                return new RetGerarIdOperacaoTransporte
                {
                    Versao = "1.00",
                    Sucesso = false,
                    Mensagem = mensagem,
                    Erros = string.Empty
                }.GerarXML();
            }

            if (Regex.IsMatch(root.InnerText?.Trim() ?? "", @"^[0-9]{12}$"))
            {
                return CriarRetornoSucesso(root.InnerText.Trim()).GerarXML();
            }

            throw new InvalidOperationException("A API ANTT retornou conteúdo inesperado ao gerar o IdOperacaoTransporte: " + retorno.OuterXml);
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço
        /// </summary>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] Xml.CIOT.GerarIdOperacaoTransporte xml, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            InicializarServico(NormalizarEnvio(xml), configuracao);
            Executar();
        }

        /// <summary>
        /// Recupera o XML processado da geração do identificador da operação de transporte no formato string para consumo por linguagens que acessam a biblioteca via COM/Interop.
        /// </summary>
        /// <returns>XML de distribuição no formato GerarIdOperacaoTransporteProc.</returns>
        [ComVisible(true)]
        public string GetGerarIdOperacaoTransporteProcResult() => GerarIdOperacaoTransporteProcResult.GerarXML().OuterXml;

        /// <summary>
        /// Recupera o XML de distribuição da geração do identificador da operação de transporte no formato string a partir do identificador da operação, permitindo acesso por linguagens que utilizam COM/Interop.
        /// </summary>
        /// <param name="idOperacaoTransporte">Identificador da operação de transporte para retornar o XML de distribuição correspondente.</param>
        /// <returns>XML de distribuição no formato GerarIdOperacaoTransporteProc.</returns>
        [ComVisible(true)]
        public string GetGerarIdOperacaoTransporteProcResults(string idOperacaoTransporte)
        {
            var retornar = "";

            if (GerarIdOperacaoTransporteProcResults.Count > 0)
            {
                retornar = GerarIdOperacaoTransporteProcResults[idOperacaoTransporte].GerarXML().OuterXml;
            }

            return retornar;
        }
#endif

        #region Public Methods

        /// <summary>
        /// Grava o XML de distribuição da geração do identificador da operação de transporte em uma pasta do disco, gerando automaticamente o nome do arquivo com base no conteúdo retornado pela API.
        /// </summary>
        /// <param name="pasta">Pasta onde o XML de distribuição será gravado.</param>
        public void GravarXmlDistribuicao(string pasta)
        {
            try
            {
                var gerarIdOperacaoTransporteProc = GerarIdOperacaoTransporteProcResult;
                GravarXmlDistribuicao(pasta, gerarIdOperacaoTransporteProc.NomeArquivoDistribuicao, gerarIdOperacaoTransporteProc.GerarXML().OuterXml);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// Grava o XML de distribuição da geração do identificador da operação de transporte em um stream informado pelo consumidor, permitindo manipulação em memória ou persistência customizada.
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

                var value = GerarIdOperacaoTransporteProcResult.GerarXML().OuterXml;
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

        private static Xml.CIOT.GerarIdOperacaoTransporte NormalizarEnvio(Xml.CIOT.GerarIdOperacaoTransporte xml)
        {
            if (xml == null)
            {
                throw new ArgumentNullException(nameof(xml));
            }

            if (!Regex.IsMatch(xml.CpfCnpj ?? "", @"^[0-9.\-/\s]+$"))
            {
                throw new ArgumentException("CPF/CNPJ deve conter somente dígitos ou caracteres de máscara.", nameof(xml));
            }

            xml.CpfCnpj = Regex.Replace(xml.CpfCnpj, @"[^0-9]", "");
            if (!Regex.IsMatch(xml.CpfCnpj, @"^[0-9]{11}$|^[0-9]{14}$"))
            {
                throw new ArgumentException("CPF/CNPJ deve conter 11 ou 14 dígitos.", nameof(xml));
            }

            xml.Versao = "1.00";
            return xml;
        }

        private static string ObterValor(XmlElement root, string nome)
        {
            if (root.LocalName == nome)
            {
                return root.InnerText;
            }

            var nodes = root.GetElementsByTagName(nome);
            return nodes.Count > 0 ? nodes[0].InnerText : null;
        }

        private static RetGerarIdOperacaoTransporte CriarRetornoSucesso(string ciot)
        {
            return new RetGerarIdOperacaoTransporte
            {
                Versao = "1.00",
                Sucesso = true,
                Mensagem = "CIOT gerado com sucesso",
                Dados = new DadosGerarIdOperacaoTransporte
                {
                    CIOT = ciot
                },
                Erros = string.Empty
            };
        }

        private static RetGerarIdOperacaoTransporte CriarRetornoANTT(XmlElement root)
        {
            bool.TryParse(ObterValor(root, "Sucesso"), out var sucesso);

            return new RetGerarIdOperacaoTransporte
            {
                Versao = "1.00",
                Sucesso = sucesso,
                Mensagem = ObterValor(root, "Mensagem"),
                Dados = new DadosGerarIdOperacaoTransporte
                {
                    CIOT = ObterValor(root, "CIOT"),
                    CpfCnpj = ObterValor(root, "CpfCnpj"),
                    DataGeracao = ObterValor(root, "DataGeracao")
                },
                Erros = ObterValor(root, "Erros") ?? string.Empty
            };
        }
    }
}
