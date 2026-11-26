#if INTEROP
using System.Runtime.InteropServices;
#endif
using GeradorCIOTShared;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using Newtonsoft.Json.Linq;
using System;
using System.Linq;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.Schema;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml;
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
    public class GerarIdOperacaoTransporte : ServicoBase, IInteropService<Xml.CIOT.GerarIdOperacaoTransporte>
    {
        #region Private Fields

        private const string MensagemSucesso = "CIOT gerado com sucesso";
        private const string MensagemSucessoInconsistente =
            "A ANTT informou sucesso, mas o retorno não confirmou um IdOperacaoTransporte válido de 12 dígitos.";
        private const string NamespaceJson = "http://james.newtonking.com/projects/json";
        private static readonly string[] NomesIdentificador = { "CIOT", "IdOperacaoTransporte", "idOperacaoTransporte" };
        private static readonly string[] CamposRetornoSimples = { "CIOT", "IdOperacaoTransporte", "idOperacaoTransporte", "CpfCnpj", "DataGeracao", "Mensagem", "Erros" };

        private Xml.CIOT.GerarIdOperacaoTransporte envio;
        private readonly System.Collections.Generic.Dictionary<string, GerarIdOperacaoTransporteProc> GerarIdOperacaoTransporteProcs = new System.Collections.Generic.Dictionary<string, GerarIdOperacaoTransporteProc>();

        #endregion Private Fields

        #region Public Properties

        /// <summary>
        /// Objeto do XML de envio
        /// </summary>
        public Xml.CIOT.GerarIdOperacaoTransporte Envio => ObterEnvio(ref envio);

        /// <summary>
        /// Resultado do serviço
        /// </summary>
        public RetGerarIdOperacaoTransporte Result => ObterResult<RetGerarIdOperacaoTransporte>();

        /// <summary>
        /// Propriedade contendo o XML da geração do identificador da operação de transporte com o retorno da API anexado para geração do arquivo de distribuição.
        /// A chave do dicionário é o identificador da operação retornado no campo <c>IdOperacaoTransporte</c>.
        /// </summary>
        [System.Runtime.InteropServices.ComVisible(false)]
        public System.Collections.Generic.Dictionary<string, GerarIdOperacaoTransporteProc> GerarIdOperacaoTransporteProcResults =>
            ObterProcResults(Result);

        /// <summary>
        /// Recupera o conteúdo processado da geração do identificador da operação de transporte, combinando o XML de envio com o XML de retorno para geração do arquivo de distribuição.
        /// </summary>
        public GerarIdOperacaoTransporteProc GerarIdOperacaoTransporteProcResult
        {
            get
            {
                var retorno = Result;
                return ObterProcResults(retorno)[retorno.IdOperacaoTransporte];
            }
        }

        #endregion Public Properties

        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTGerarIdOperacaoTransporte;

        /// <inheritdoc />
        protected override string NomeRootRetorno => nameof(RetGerarIdOperacaoTransporte);

        /// <inheritdoc />
        protected override XMLBase XmlEnvio => Envio;

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
            RetornoWSXML = null;
            RetornoWSString = null;
            RetornoWSRawString = retorno;
            HttpStatusCode = default;

            if (string.IsNullOrWhiteSpace(retorno))
            {
                throw new InvalidOperationException("A API ANTT retornou conteúdo vazio ao gerar o IdOperacaoTransporte.");
            }

            XmlDocument doc;
            var conteudo = retorno.Trim();

            if (RetGerarIdOperacaoTransporte.IdValido(conteudo))
            {
                doc = new XmlDocument();
                doc.LoadXml("<temp><IdOperacaoTransporte>" + conteudo + "</IdOperacaoTransporte></temp>");
            }
            else
            {
                try
                {
                    var objeto = LerJsonANTT(conteudo);
                    using (var leitor = objeto.CreateReader())
                    {
                        var conversor = new XmlNodeConverter
                        {
                            DeserializeRootElementName = "temp",
                            EncodeSpecialCharacters = true,
                            WriteArrayAttribute = true
                        };
                        doc = JsonSerializer.CreateDefault(new JsonSerializerSettings { Converters = { conversor } })
                            .Deserialize<XmlDocument>(leitor);
                    }
                }
                catch (Exception ex) when (ex is JsonException || ex is XmlException || ex is ArgumentException)
                {
                    throw new InvalidOperationException("A ANTT retornou conteúdo inválido ao gerar o IdOperacaoTransporte.");
                }
            }

            if (doc?.DocumentElement == null)
            {
                throw new InvalidOperationException("A API ANTT retornou conteúdo inválido ao gerar o IdOperacaoTransporte.");
            }

            RetornoWSXML = doc;
            try
            {
                return Result;
            }
            catch
            {
                RetornoWSXML = null;
                RetornoWSString = null;
                throw;
            }
        }

        /// <inheritdoc />
        protected override XmlDocument CriarXMLRetornoTipado()
        {
            var retorno = NormalizarRetornoANTT();
            var status = (int)HttpStatusCode;
            if (status != 0 && (status < 200 || status >= 300) &&
                EhSucessoXml(ObterValor(retorno.DocumentElement, "Sucesso")))
            {
                return CriarErroRetorno("1.00", "RETORNO_HTTP_INVALIDO",
                    $"A ANTT retornou HTTP {status} ao gerar o IdOperacaoTransporte.").GerarXML();
            }

            return retorno;
        }

        private XmlDocument NormalizarRetornoANTT()
        {
            var retorno = RetornoWSXML;
            var root = retorno?.DocumentElement;

            if (root == null)
            {
                throw new InvalidOperationException("A API ANTT retornou conteúdo vazio ao gerar o IdOperacaoTransporte.");
            }

            if (root.LocalName == nameof(RetGerarIdOperacaoTransporte))
            {
                retorno = RemoverTimestampInvalido(retorno);
                root = retorno.DocumentElement;
                var tipado = new RetGerarIdOperacaoTransporte().LerXML<RetGerarIdOperacaoTransporte>(retorno);
                if (tipado.Sucesso != true)
                {
                    return retorno;
                }

                if (AplicarRejeicaoExplicita(tipado))
                {
                    return tipado.GerarXML();
                }

                if (!EhSucessoXml(ObterValor(root, "Sucesso")))
                {
                    return CriarErroIndicadorSucesso(tipado.Versao).GerarXML();
                }

                if (tipado.Codigo != "110" || tipado.IdOperacaoTransporte != ObterIdEstruturado(root, false))
                {
                    return MarcarSucessoInconsistente(tipado).GerarXML();
                }

                var validado = ValidarDataGeracao(tipado, root["Dados", root.NamespaceURI]);
                return ReferenceEquals(tipado, validado) ? retorno : validado.GerarXML();
            }

            JObject objeto = null;
            var bruto = RetornoWSRawString?.Trim();
            if (root.LocalName == "temp" && !string.IsNullOrEmpty(bruto) &&
                bruto[0] != '<' && !RetGerarIdOperacaoTransporte.IdValido(bruto))
            {
                try
                {
                    objeto = LerJsonANTT(bruto);
                }
                catch (JsonException)
                {
                    return CriarErroRetorno("1.00", "RETORNO_JSON_INVALIDO",
                        "A ANTT retornou um JSON inválido ao gerar o IdOperacaoTransporte.").GerarXML();
                }
            }

            if (root.LocalName == "temp" && root["error"] != null)
            {
                var retornoErro = RemoverTimestampInvalido(base.CriarXMLRetornoTipado());
                return new RetGerarIdOperacaoTransporte().LerXML<RetGerarIdOperacaoTransporte>(retornoErro).GerarXML();
            }

            if (root.LocalName == "temp" && ObterElementos(root, "Sucesso").Length > 0)
            {
                return CriarRetornoANTT(root, objeto).GerarXML();
            }

            var mensagem = ObterValor(root, "Mensagem");
            var mensagemErro = ObterMensagemErro(root);
            if (string.IsNullOrWhiteSpace(mensagemErro) && mensagem != MensagemSucesso)
            {
                mensagemErro = mensagem;
            }

            var erros = ObterErros(root);
            if (!string.IsNullOrWhiteSpace(mensagemErro) || !string.IsNullOrWhiteSpace(erros))
            {
                return new RetGerarIdOperacaoTransporte
                {
                    Versao = "1.00",
                    Sucesso = false,
                    Mensagem = string.IsNullOrWhiteSpace(mensagemErro) ? erros : mensagemErro,
                    Erros = erros
                }.GerarXML();
            }

            if (root.LocalName == "temp" && EhRetornoSimples(root, objeto))
            {
                var id = ObterValor(root, NomesIdentificador);
                if (RetGerarIdOperacaoTransporte.IdValido(id))
                {
                    var resultado = CriarRetornoSucesso(id, ObterValor(root, "CpfCnpj"), ObterValor(root, "DataGeracao"));
                    return ValidarDataGeracao(resultado, root, objeto).GerarXML();
                }
            }

            if (EhRaizEscalarDoIdentificador(root) && EhValorSimples(root) &&
                RetGerarIdOperacaoTransporte.IdValido(root.InnerText.Trim()))
            {
                return CriarRetornoSucesso(root.InnerText.Trim()).GerarXML();
            }

            throw new InvalidOperationException("A ANTT retornou conteúdo inesperado ao gerar o IdOperacaoTransporte.");
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço
        /// </summary>
        [ComVisible(true)]
        public void Executar(Xml.CIOT.GerarIdOperacaoTransporte xml, Configuracao configuracao)
        {
            LimparExecucaoAnterior();
            envio = null;
            try
            {
                InicializarServico(NormalizarEnvio(xml), configuracao);
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


        /// <inheritdoc />
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            LimparExecucaoAnterior();
            if (Configuracoes.TipoAmbiente == TipoAmbiente.Producao)
            {
                // O componente ANTT não expõe o endpoint ou o HTTP efetivo; não inventar esses dados.
                Utility.SessaoObservacaoDisponibilidade.InformarTransporte(string.Empty, "ComponenteExterno");
                try
                {
                    RetornoWSRawString = GerarCIOTProducao(Envio.CpfCnpj);
                }
                catch (Exception ex)
                {
                    throw new InvalidOperationException($"Não foi possível executar o gerador de IdOperacaoTransporte da ANTT (HRESULT 0x{ex.HResult:X8}).");
                }

                ProcessarRetornoANTT(RetornoWSRawString);
                return;
            }

            base.Executar();
        }

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

        private void LimparExecucaoAnterior()
        {
            LimparRetornoWS();
            GerarIdOperacaoTransporteProcs.Clear();
        }

        private System.Collections.Generic.Dictionary<string, GerarIdOperacaoTransporteProc> ObterProcResults(RetGerarIdOperacaoTransporte retorno)
        {
            if (retorno?.Codigo != "110")
            {
                throw new InvalidOperationException("Não é possível gerar o XML de distribuição sem um retorno de sucesso com IdOperacaoTransporte válido de 12 dígitos. Consulte o resultado do serviço.");
            }

            if (GerarIdOperacaoTransporteProcs.TryGetValue(retorno.IdOperacaoTransporte, out var proc))
            {
                proc.RetGerarIdOperacaoTransporte = retorno;
            }
            else
            {
                GerarIdOperacaoTransporteProcs.Add(retorno.IdOperacaoTransporte, new GerarIdOperacaoTransporteProc
                {
                    Versao = Configuracoes.SchemaVersao,
                    GerarIdOperacaoTransporte = Envio,
                    RetGerarIdOperacaoTransporte = retorno
                });
            }

            return GerarIdOperacaoTransporteProcs;
        }

        private static XmlElement[] ObterElementos(XmlElement pai, params string[] nomes) =>
            pai.ChildNodes.OfType<XmlElement>()
                .Where(elemento => elemento.NamespaceURI == pai.NamespaceURI && nomes.Contains(elemento.LocalName))
                .ToArray();

        private static bool EhValorSimples(XmlElement elemento) =>
            !elemento.ChildNodes.OfType<XmlElement>().Any() &&
            elemento.GetAttribute("Array", NamespaceJson) != "true";

        private static bool EhRaizEscalarDoIdentificador(XmlElement root)
        {
            if (root.LocalName == "string")
            {
                return root.NamespaceURI.Length == 0 || root.NamespaceURI == "http://schemas.microsoft.com/2003/10/Serialization/";
            }

            return (root.LocalName == "IdOperacaoTransporte" || root.LocalName == "idOperacaoTransporte") &&
                (root.NamespaceURI.Length == 0 || root.NamespaceURI == CIOTNamespace.PortalANTT);
        }

        private static bool TemTextoAvulso(XmlElement elemento) =>
            elemento.ChildNodes.OfType<XmlCharacterData>()
                .Any(no => (no.NodeType == XmlNodeType.Text || no.NodeType == XmlNodeType.CDATA) &&
                    !string.IsNullOrWhiteSpace(no.Value)) ||
            elemento.ChildNodes.OfType<XmlElement>()
                .Any(no => XmlConvert.DecodeName(no.LocalName).StartsWith("#", StringComparison.Ordinal));

        private static bool EhRetornoSimples(XmlElement root, JObject objeto)
        {
            var elementos = root.ChildNodes.OfType<XmlElement>().ToArray();
            var conhecidos = ObterElementos(root, CamposRetornoSimples);
            var mensagens = ObterElementos(root, "Mensagem");
            var erros = ObterElementos(root, "Erros");
            return (objeto == null || (objeto.Properties().All(campo => CamposRetornoSimples.Contains(campo.Name)) &&
                    EhIdentificadorJsonUnico(objeto))) &&
                !TemTextoAvulso(root) && elementos.Length == conhecidos.Length &&
                conhecidos.Where(elemento => elemento.LocalName != "DataGeracao").All(EhValorSimples) &&
                (mensagens.Length == 0 || (mensagens.Length == 1 && mensagens[0].InnerText == MensagemSucesso)) &&
                (erros.Length == 0 || (erros.Length == 1 && erros[0].InnerText.Length == 0 &&
                    (objeto == null || objeto["Erros"]?.Type == JTokenType.String))) &&
                ObterElementos(root, "CpfCnpj").Length <= 1;
        }

        private static string ObterValor(XmlElement pai, params string[] nomes)
        {
            var elementos = ObterElementos(pai, nomes);
            return elementos.Length == 1 && EhValorSimples(elementos[0]) ? elementos[0].InnerText : null;
        }

        private static string ObterIdEstruturado(XmlElement root, bool aceitarCiotNaRaiz)
        {
            if (TemTextoAvulso(root))
            {
                return null;
            }

            var grupos = ObterElementos(root, "Dados");
            var idsNaRaiz = ObterElementos(root, NomesIdentificador);
            if (grupos.Length == 0 && aceitarCiotNaRaiz)
            {
                return idsNaRaiz.Length == 1 ? ObterValor(root, "CIOT") : null;
            }

            if (grupos.Length != 1 || idsNaRaiz.Length != 0 ||
                grupos[0].GetAttribute("Array", NamespaceJson) == "true" ||
                TemTextoAvulso(grupos[0]) ||
                ObterElementos(grupos[0], "IdOperacaoTransporte", "idOperacaoTransporte").Length != 0)
            {
                return null;
            }

            return ObterValor(grupos[0], "CIOT");
        }

        /// <summary>
        /// Gerar CIOT em produção utilizando a biblioteca disponibilizada pela ANTT.
        /// </summary>
        /// <param name="cpfCnpj">CPF ou CNPJ sem máscara.</param>
        /// <returns>Retorno bruto disponibilizado pela biblioteca da ANTT.</returns>
        protected virtual string GerarCIOTProducao(string cpfCnpj) => new GeradorCIOTService_v3().GerarCIOT(cpfCnpj);

        private static RetGerarIdOperacaoTransporte CriarRetornoSucesso(string ciot, string cpfCnpj = null, string dataGeracao = null)
        {
            return new RetGerarIdOperacaoTransporte
            {
                Versao = "1.00",
                Sucesso = true,
                Mensagem = MensagemSucesso,
                Dados = new DadosGerarIdOperacaoTransporte
                {
                    CIOT = ciot,
                    CpfCnpj = cpfCnpj,
                    DataGeracao = dataGeracao
                },
                Erros = string.Empty
            };
        }

        private static RetGerarIdOperacaoTransporte CriarRetornoANTT(XmlElement root, JObject objeto)
        {
            var indicador = objeto?["Sucesso"];
            var indicadorValido = objeto == null || indicador?.Type == JTokenType.Boolean || indicador?.Type == JTokenType.String;
            var sucesso = indicadorValido &&
                bool.TryParse(ObterValor(root, "Sucesso"), out var sucessoDeclarado) && sucessoDeclarado;
            var grupoDados = root["Dados", root.NamespaceURI];
            var dados = grupoDados ?? root;
            var dadosJson = grupoDados == null ? objeto : objeto?["Dados"] as JObject;
            var id = EhIdEstruturadoJsonValido(objeto, dadosJson, grupoDados != null) ? ObterIdEstruturado(root, true) : null;

            var retorno = new RetGerarIdOperacaoTransporte
            {
                Versao = "1.00",
                Sucesso = sucesso,
                Mensagem = ObterValor(root, "Mensagem"),
                Dados = new DadosGerarIdOperacaoTransporte
                {
                    CIOT = id,
                    CpfCnpj = ObterValor(dados, "CpfCnpj"),
                    DataGeracao = ObterValor(dados, "DataGeracao")
                },
                Erros = ObterErros(root)
            };

            if (AplicarRejeicaoExplicita(retorno, ObterMensagemErro(root)))
            {
                return retorno;
            }

            if (!indicadorValido)
            {
                return CriarErroIndicadorSucesso(retorno.Versao);
            }

            return sucesso && retorno.Codigo != "110"
                ? MarcarSucessoInconsistente(retorno)
                : ValidarDataGeracao(retorno, dados, dadosJson);
        }

        private static bool EhSucessoXml(string valor)
        {
            var texto = valor?.Trim();
            return texto == "true" || texto == "1";
        }

        private static bool EhIdEstruturadoJsonValido(JObject objeto, JObject dados, bool temGrupoDados)
        {
            if (objeto == null)
            {
                return true;
            }

            return (temGrupoDados ? !NomesIdentificador.Any(nome => objeto.Property(nome) != null) : objeto.Property("Dados") == null) &&
                dados != null && EhIdentificadorJsonUnico(dados, "CIOT");
        }

        private static bool EhIdentificadorJsonUnico(JObject objeto, string nomeEsperado = null)
        {
            var campos = objeto.Properties().Where(campo => NomesIdentificador.Contains(campo.Name)).ToArray();
            return campos.Length == 1 && (nomeEsperado == null || campos[0].Name == nomeEsperado) &&
                (campos[0].Value.Type == JTokenType.String || campos[0].Value.Type == JTokenType.Integer);
        }

        private static string ObterMensagemErro(XmlElement root)
        {
            var mensagem = ObterErros(root, "Message");
            return string.IsNullOrWhiteSpace(mensagem) ? ObterErros(root, "message") : mensagem;
        }

        private static string ObterErros(XmlElement root, string nome = "Erros") =>
            string.Join(Environment.NewLine, ObterElementos(root, nome).Select(erro => erro.InnerText));

        private static bool AplicarRejeicaoExplicita(RetGerarIdOperacaoTransporte retorno, string mensagemErro = null)
        {
            var precisaMensagemDeRejeicao = string.IsNullOrWhiteSpace(retorno.Mensagem) || retorno.Mensagem == MensagemSucesso;
            if (!string.IsNullOrWhiteSpace(mensagemErro))
            {
                if (retorno.Sucesso == true || precisaMensagemDeRejeicao)
                {
                    retorno.Mensagem = mensagemErro;
                }
            }
            else if (string.IsNullOrWhiteSpace(retorno.Erros))
            {
                return false;
            }
            else if (precisaMensagemDeRejeicao)
            {
                retorno.Mensagem = retorno.Erros;
            }

            retorno.Sucesso = false;
            return true;
        }

        private static RetGerarIdOperacaoTransporte ValidarDataGeracao(RetGerarIdOperacaoTransporte retorno, XmlElement origem, JObject dadosJson = null)
        {
            if (retorno.Sucesso != true)
            {
                return retorno;
            }

            if (DataGeracaoTemEstruturaInvalida(origem, dadosJson))
            {
                return CriarErroDataGeracao(retorno.Versao);
            }

            var dataGeracao = retorno.Dados?.DataGeracao;
            return dataGeracao == null || EhDataHoraXmlValida(dataGeracao)
                ? retorno
                : CriarErroDataGeracao(retorno.Versao);
        }

        private static XmlDocument RemoverTimestampInvalido(XmlDocument retorno)
        {
            var erro = retorno.DocumentElement["temp", CIOTNamespace.PortalANTT];
            if (erro == null)
            {
                return retorno;
            }

            var timestamps = ObterElementos(erro, "timestamp");
            if (timestamps.Length == 0 || (timestamps.Length == 1 &&
                EhValorSimples(timestamps[0]) && EhDataHoraXmlValida(timestamps[0].InnerText)))
            {
                return retorno;
            }

            var copia = (XmlDocument)retorno.CloneNode(true);
            var erroNormalizado = copia.DocumentElement["temp", CIOTNamespace.PortalANTT];
            foreach (var timestamp in ObterElementos(erroNormalizado, "timestamp"))
            {
                erroNormalizado.RemoveChild(timestamp);
            }

            return copia;
        }

        private static bool EhDataHoraXmlValida(string valor)
        {
            try
            {
                XmlSchemaType.GetBuiltInSimpleType(XmlTypeCode.DateTime).Datatype.ParseValue(valor, null, null);
                return true;
            }
            catch (XmlSchemaException)
            {
                return false;
            }
        }

        private static bool DataGeracaoTemEstruturaInvalida(XmlElement origem, JObject dadosJson)
        {
            var datas = ObterElementos(origem, "DataGeracao");
            if (datas.Length > 1 || (datas.Length == 1 && !EhValorSimples(datas[0])))
            {
                return true;
            }

            // A conversão JSON/XML pode omitir listas vazias ou representar uma lista unitária como texto.
            var data = dadosJson?["DataGeracao"];
            return data != null && data.Type != JTokenType.String && data.Type != JTokenType.Date;
        }

        private static JObject LerJsonANTT(string conteudo) =>
            JObject.Parse(conteudo, new JsonLoadSettings { DuplicatePropertyNameHandling = DuplicatePropertyNameHandling.Error });

        private static RetGerarIdOperacaoTransporte CriarErroDataGeracao(string versao) =>
            CriarErroRetorno(versao, "DATA_GERACAO_INVALIDA", "A DataGeracao no retorno da ANTT é inválida para o tipo xs:dateTime.");

        private static RetGerarIdOperacaoTransporte CriarErroIndicadorSucesso(string versao) =>
            CriarErroRetorno(versao, "INDICADOR_SUCESSO_INVALIDO", "O campo Sucesso no retorno da ANTT é inválido ou ambíguo.");

        private static RetGerarIdOperacaoTransporte CriarErroRetorno(string versao, string codigo, string mensagem) =>
            new RetGerarIdOperacaoTransporte
            {
                Versao = versao,
                Temp = new Temp
                {
                    Error = codigo,
                    Message = mensagem
                }
            };

        private static RetGerarIdOperacaoTransporte MarcarSucessoInconsistente(RetGerarIdOperacaoTransporte retorno)
        {
            retorno.Sucesso = false;
            retorno.Mensagem = MensagemSucessoInconsistente;
            return retorno;
        }
    }
}
